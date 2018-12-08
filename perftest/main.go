package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"math/rand"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/websocket"
)

// cli flags
var (
	addrsFlag          = flag.String("addrs", "localhost:7777", "CSV of setml game addresses")
	gamesPerServerFlag = flag.Int("games-per-server", 5, "Number of games per server")
	playersPerGameFlag = flag.Int("players-per-game", 5, "Number of players per game")
	roundsFlag         = flag.Int("rounds", 1, "Number of rounds of games to simulate")
)

type Server struct {
	Name     string
	NumGames int
}

type Game struct {
	Id         string
	Complete   bool
	NumPlayers int
	Wg         *sync.WaitGroup
}

type Player struct {
	Id     string
	Token  string
	Client *http.Client
}

type PlayerGame struct {
	Conn       *websocket.Conn
	Player     *Player
	Game       *Game
	Board      []Card
	NumUpdated int
}

type Card struct {
	Attrs []int
}

func NewCard(cardId int) (Card, error) {
	if cardId < 0 || cardId > 81 {
		return Card{}, fmt.Errorf("Invalid card id: %d", cardId)
	}
	if cardId == 81 {
		return Card{}, nil
	}
	attrs := make([]int, 4)
	n := cardId
	for i := 3; i >= 0; i-- {
		if n == 0 {
			break
		}
		attrs[i] = n % 3
		n = n / 3
	}
	return Card{attrs}, nil
}

func CardToInt(card Card) int {
	return card.Attrs[0]*27 +
		card.Attrs[1]*9 +
		card.Attrs[2]*3 +
		card.Attrs[3]
}

func IsSet(card0, card1, card2 Card) bool {
	if len(card0.Attrs) != 4 ||
		len(card1.Attrs) != 4 ||
		len(card2.Attrs) != 4 {
		return false
	}
	attrSet := make([]bool, 4)
	for i := 0; i < 4; i++ {
		attrSet[i] = ((card0.Attrs[i] == card1.Attrs[i] &&
			card1.Attrs[i] == card2.Attrs[i]) ||
			(card0.Attrs[i] != card1.Attrs[i] &&
				card1.Attrs[i] != card2.Attrs[i] &&
				card0.Attrs[i] != card2.Attrs[i]))
	}
	return attrSet[0] && attrSet[1] && attrSet[2] && attrSet[3]
}

func randRange(min, max int) []int {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	l := max - min + 1
	a := make([]int, l)
	p := r.Perm(l)
	for i, rIdx := range p {
		a[i] = rIdx + min
	}
	return a
}

func FindSet(cards []Card) []int {
	return FindRandSet(cards)
}

func FindRandSet(cards []Card) []int {
	l := len(cards)
	if l < 3 {
		return []int{}
	}
	r := randRange(0, l-1)
	for i := 0; i < l-2; i++ { // 0 -  9
		for j := i + 1; j < l-1; j++ { // 1 - 10
			for k := j + 1; k < l; k++ { // 2 - 11
				if IsSet(cards[r[i]], cards[r[j]], cards[r[k]]) {
					return []int{
						r[i], CardToInt(cards[r[i]]),
						r[j], CardToInt(cards[r[j]]),
						r[k], CardToInt(cards[r[k]]),
					}
				}
			}
		}
	}
	return []int{}
}

// returns slice of alternating board idx, card_id x 3
func FindFirstSet(cards []Card) []int {
	l := len(cards)
	if l < 3 {
		return []int{}
	}
	for i := 0; i < l-2; i++ { // 0 -  9
		for j := i + 1; j < l-1; j++ { // 1 - 10
			for k := j + 1; k < l; k++ { // 2 - 11
				if IsSet(cards[i], cards[j], cards[k]) {
					return []int{
						i, CardToInt(cards[i]),
						j, CardToInt(cards[j]),
						k, CardToInt(cards[k]),
					}
				}
			}
		}
	}
	return []int{}
}

func ServerGameStatus(data map[string]interface{}) (string, error) {
	gameUpdate, ok := data["game_update"].(map[string]interface{})
	if !ok {
		return "", errors.New("No game_update in server_game message")
	}

	status, ok := gameUpdate["status"].(string)
	if !ok {
		return "", errors.New("No status in gameUpdate message")
	}

	return status, nil
}

func ServerGameUpdateStatus(data map[string]interface{}) (string, error) {
	status, ok := data["status"].(string)
	if !ok {
		return "", errors.New("No status in server_game_update message")
	}

	return status, nil
}

func ServerGameBoard(data map[string]interface{}) ([]Card, error) {
	board := make([]Card, 12)
	boardCardData, ok := data["board_card_data"].([]interface{})
	if !ok {
		return board, errors.New("No board_card_data in server_game message")
	}

	if len(boardCardData) != 12 {
		return board, errors.New("board_card_data is not length 12")
	}

	for _, bcdi := range boardCardData {
		bcd, ok := bcdi.(map[string]interface{})
		if !ok {
			return board, errors.New("board_card_data element is not a map of strings")
		}

		idx, ok := bcd["idx"].(float64)
		if !ok {
			return board, errors.New("No idx in board_card_data message")
		}

		cardId, ok := bcd["card_id"].(float64)
		if !ok {
			return board, errors.New("No card_id in board_card_data message")
		}

		card, err := NewCard(int(cardId))
		if err != nil {
			return board, fmt.Errorf("Unable to create card: %s", err)
		}
		board[int(idx)] = card
	}

	return board, nil
}

func ServerBoardCard(data map[string]interface{}, board []Card) ([]Card, error) {
	idx, ok := data["idx"].(float64)
	if !ok {
		return board, errors.New("No idx in server_board_card message")
	}

	cardId, ok := data["card_id"].(float64)
	if !ok {
		return board, errors.New("No card_id in server_board_card message")
	}

	card, err := NewCard(int(cardId))
	if err != nil {
		return board, fmt.Errorf("Unable to create card: %s", err)
	}
	board[int(idx)] = card

	return board, nil
}

func shouldShuffle() bool {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	return r.Float64() >= 0.333
}

func randSleep(minMs, maxMs int) {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	n := r.Int31n(int32(maxMs)) + int32(minMs)
	time.Sleep(time.Duration(n) * time.Millisecond)
}

func (pg *PlayerGame) HandleMessage(message []byte) (bool, error) {
	data := make(map[string]interface{})
	err := json.Unmarshal(message, &data)
	if err != nil {
		return false, err
	}

	msgType, ok := data["type"].(string)
	if !ok {
		return false, errors.New("No type in message")
	}

	switch msgType {
	case "server_game":
		status, err := ServerGameStatus(data)
		if err != nil {
			return false, err
		}
		switch status {
		case "new":
			err = pg.StartGame()
			if err != nil {
				return false, fmt.Errorf("Error starting game: %s", err)
			}
		case "started":
			board, err := ServerGameBoard(data)
			if err != nil {
				return false, fmt.Errorf("Error getting board data: %s", err)
			}
			pg.Board = board

			set := FindSet(pg.Board)
			randSleep(250, 500)
			if len(set) == 6 {
				pg.MakeMove(set)
			} else if shouldShuffle() {
				pg.ShuffleBoard()
			}
		case "complete":
			return true, nil
		default:
			return false, fmt.Errorf("Invalid game status: %s", status)
		}
	case "server_board_card":
		board, err := ServerBoardCard(data, pg.Board)
		if err != nil {
			return false, fmt.Errorf("Update to update board: %s", err)
		}
		pg.Board = board
		pg.NumUpdated += 1

		if pg.NumUpdated >= 3 {
			pg.NumUpdated = 0
			set := FindSet(pg.Board)
			randSleep(250, 500)
			if len(set) == 6 {
				pg.MakeMove(set)
			} else if shouldShuffle() {
				pg.ShuffleBoard()
			}
		}
	case "server_game_update":
		status, err := ServerGameUpdateStatus(data)
		if err != nil {
			return false, err
		}
		if status == "complete" {
			return true, nil
		}
	case "server_presence", "server_move_info", "server_shuffles":
		// ignore
	default:
		log.Printf("Unhandled message type: %s", msgType)
	}

	return false, nil
}

func (pg *PlayerGame) Send(body string, args ...interface{}) error {
	return pg.Conn.WriteMessage(websocket.TextMessage, []byte(fmt.Sprintf(body, args...)))
}

func (pg *PlayerGame) StartGame() error {
	return pg.Send(`{
		"type": "client_start_game",
		"token": "%s"
	}`, pg.Player.Token)
}

func (pg *PlayerGame) MakeMove(set []int) error {
	return pg.Send(`{
		"type": "client_move",
		"token": "%s",
		"card0": {
			"type": "card_data",
			"idx": %d,
			"card_id": %d
		},
		"card1": {
			"type": "card_data",
			"idx": %d,
			"card_id": %d
		},
		"card2": {
			"type": "card_data",
			"idx": %d,
			"card_id": %d
		}
	}`, pg.Player.Token, set[0], set[1], set[2], set[3], set[4], set[5])
}

func (pg *PlayerGame) ShuffleBoard() error {
	return pg.Send(`{
		"type": "client_shuffle",
		"token": "%s"
	}`, pg.Player.Token)
}

func (pg *PlayerGame) ReadPump() {
	defer pg.Conn.Close()
	defer pg.Game.Wg.Done()
	pg.Game.Wg.Add(1)

	if pg.Game.Complete {
		return
	}

	ch := make(chan []byte)
	tickChan := time.NewTicker(time.Millisecond * 100).C

	go func() {
		for {
			_, message, err := pg.Conn.ReadMessage()
			if err != nil {
				if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway, websocket.CloseAbnormalClosure) {
					log.Printf("error: %v", err)
				}
				break
			}

			ch <- bytes.TrimSpace(message)
		}
	}()

	for {
		if pg.Game.Complete {
			break
		}

		select {
		case message := <-ch:
			quit, err := pg.HandleMessage(message)
			if err != nil {
				log.Printf("Error handling message: %s", err)
			}

			if quit {
				pg.Game.Complete = true
			}
		case <-tickChan:
			// ignore
		}
	}
}

var tokenRe = regexp.MustCompile(`<meta name="token" content="([0-9a-f]+)"/>`)
var gameIdRe = regexp.MustCompile(`<title>SetML: ([0-9a-z]+)</title>`)
var playerIdRe = regexp.MustCompile(`<meta name="player_id" content="([0-9]+)"/>`)

func parseValue(re *regexp.Regexp, s string) (string, error) {
	results := re.FindAllStringSubmatch(s, -1)
	if len(results) >= 1 {
		return results[0][1], nil
	} else {
		return "", fmt.Errorf("No parsed value found!")
	}
}

func createPlayerFromUrl(url string) (*Player, error) {
	jar, err := cookiejar.New(&cookiejar.Options{})
	if err != nil {
		return &Player{}, fmt.Errorf("Error building player cookie jar: %s", err)
	}

	client := &http.Client{
		Jar: jar,
	}

	player := &Player{Client: client}

	// get a token and cookie
	resp, err := player.Client.Get(url)
	if err != nil {
		return player, fmt.Errorf("Error getting token and cookie: %s", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return player, fmt.Errorf("Error requesting token and cookie, code: %d", resp.StatusCode)
	}

	bodyBytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return player, fmt.Errorf("Error reading body bytes: %s", err)
	}
	resp.Body.Close()

	bodyString := string(bodyBytes)
	token, err := parseValue(tokenRe, bodyString)
	if err != nil {
		return player, fmt.Errorf("Error parsing token: %s", err)
	}

	player.Token = token

	playerId, err := parseValue(playerIdRe, bodyString)
	if err == nil {
		player.Id = playerId
	}

	return player, nil
}

func createPlayerAndGame(server *Server) (*Player, *Game, error) {
	game := Game{Wg: &sync.WaitGroup{}}

	player, err := createPlayerFromUrl(fmt.Sprintf("http://%s/", server.Name))
	if err != nil {
		return &Player{}, &game, fmt.Errorf("Error creating player: %s", err)
	}

	data := url.Values{}
	data.Set("token", player.Token)

	req, _ := http.NewRequest("POST", fmt.Sprintf("http://%s/games", server.Name), strings.NewReader(data.Encode())) // URL-encoded payload
	req.Header.Add("Content-Type", "application/x-www-form-urlencoded")
	req.Header.Add("Content-Length", strconv.Itoa(len(data.Encode())))

	resp, err := player.Client.Do(req)
	if err != nil {
		return player, &game, fmt.Errorf("Error posting game creation: %s", err)
	}

	bodyBytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return player, &game, fmt.Errorf("Error reading game body bytes: %s", err)
	}
	resp.Body.Close()

	bodyString := string(bodyBytes)
	gameId, err := parseValue(gameIdRe, bodyString)
	if err != nil {
		return player, &game, fmt.Errorf("Error parsing gameId: %s", err)
	}
	game.Id = gameId

	playerId, err := parseValue(playerIdRe, bodyString)
	if err != nil {
		return player, &game, fmt.Errorf("Error parsing playerId: %s", err)
	}
	player.Id = playerId

	return player, &game, nil
}

func createPlayer(server *Server, game *Game) (*Player, error) {
	player, err := createPlayerFromUrl(fmt.Sprintf("http://%s/games/%s", server.Name, game.Id))
	if err != nil {
		return &Player{}, fmt.Errorf("Error creating player: %s", err)
	}
	return player, nil
}

func joinGame(server *Server, game *Game, player *Player) (*PlayerGame, error) {
	u := url.URL{Scheme: "ws", Host: server.Name, Path: fmt.Sprintf("/games/%s/ws", game.Id)}

	dialer := &websocket.Dialer{
		Proxy:            http.ProxyFromEnvironment,
		HandshakeTimeout: 45 * time.Second,
		Jar:              player.Client.Jar,
	}

	c, _, err := dialer.Dial(u.String(), nil)
	if err != nil {
		return &PlayerGame{}, err
	}

	pg := &PlayerGame{
		Conn:   c,
		Player: player,
		Game:   game,
		Board:  make([]Card, 12),
	}

	go pg.ReadPump()
	return pg, nil
}

func main() {
	flag.Parse()
	log.SetFlags(0)

	serverNames := strings.Split(*addrsFlag, ",")
	servers := make([]*Server, len(serverNames))
	for i, sn := range serverNames {
		servers[i] = &Server{Name: sn}
	}

	for round := 1; round <= *roundsFlag; round++ {
		log.Printf("Starting round %d of %d", round, *roundsFlag)
		roundGames := []*Game{}

		for _, s := range servers {
			go func(server *Server) {
				serverGames := []*Game{}
				for g := 0; g < *gamesPerServerFlag; g++ {
					player, game, err := createPlayerAndGame(server)
					if err != nil {
						log.Fatalf("Error creating player and game: %s", err)
					}
					serverGames = append(serverGames, game)
					roundGames = append(roundGames, game)

					log.Printf("%d/%d: Starting game %s (%d) on server %s", round, *roundsFlag, game.Id, server.NumGames+1, server.Name)
					_, err = joinGame(server, game, player)
					if err != nil {
						log.Fatalf("Unable to join inital game: %s", err)
					}
					server.NumGames += 1
					game.NumPlayers += 1
				}

				n := len(serverGames)
				i := 0

				for p := 0; p < *playersPerGameFlag-1; p++ {
					for g := 0; g < *gamesPerServerFlag; g++ {
						gameIdx := i % n
						game := serverGames[gameIdx]

						if !game.Complete {
							player, err := createPlayer(server, game)
							if err != nil {
								log.Fatalf("Error creating player: %s", err)
							}

							log.Printf("%d/%d: Player %d joining game %s (%d/%d) on server %s", round, *roundsFlag, game.NumPlayers+1, game.Id, gameIdx+1, len(serverGames), server.Name)
							_, err = joinGame(server, game, player)
							if err != nil {
								log.Fatalf("Unable to join existing game: %s", err)
							}
							game.NumPlayers += 1

							randSleep(1000, 2000)
						}
						i += 1
					}
				}
			}(s)
		}

		// wait for games to start
		time.Sleep(1 * time.Second)

		for _, game := range roundGames {
			game.Wg.Wait()
		}
	}
}
