open Jest;
open Expect;

open Api_messages;
open ClientApiMessages;

{
  let test_convert_player_games = a => {
    open ClientApiMessageConverter;
    let b = a |> player_games_to_json |> player_games_of_json;
    expect(a) |> toEqual(b);
  };

  describe("ClientApiMessages", () =>
    test("player_games", () =>
      test_convert_player_games([
        make_player_game(7, Game_status.New, 13, 1543797509.5927184),
        make_player_game(11, Game_status.Started, 17, 1543797519.5977184),
        make_player_game(13, Game_status.Complete, 19, 1543797719.7977184),
      ])
    )
  );
};
