--
-- Setup Table Structure
--


-- players table

drop table if exists players cascade;

create table players (
    id bigserial not null primary key,
    name character varying(255) not null default '',
    created_at timestamp without time zone default now()
);

insert into players (name) values ('andrew');


-- games table

drop sequence if exists games_id_counter_seq cascade;

create sequence games_id_counter_seq
    start with 1
    increment by 1
    no minvalue
    no maxvalue
    cache 1;

drop table if exists games cascade;

create table games (
    id bigint not null primary key,
    id_counter bigint not null,
    status character varying(10) not null default 'new',
    card_idx int not null default 0,
    created_at timestamp without time zone default now(),
    check (id <= 2176782335)
);

create unique index idx_0000 on games using btree (id_counter);


-- games_players tabe

drop table if exists games_players cascade;

create table games_players (
    game_id bigint not null references games (id)
        on delete cascade,
    player_id bigint not null references players (id)
        on delete cascade,
    presence boolean default true,
    created_at timestamp without time zone not null default now(),
    updated_at timestamp without time zone not null default now(),
    primary key (game_id, player_id)
);

create index idx_0001 on games_players using btree (game_id);
create index idx_0002 on games_players using btree (player_id);


-- game_cards table

drop table if exists game_cards cascade;

create table game_cards (
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    idx int not null,
    card_id int not null,
    check (idx >= 0 and idx < 81),
    check (card_id >= 0 and card_id < 81)
);

create unique index idx_0003 on game_cards using btree (game_id,idx);
create unique index idx_0004 on game_cards using btree (game_id,card_id);


-- board_cards table

drop table if exists board_cards cascade;

create table board_cards (
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    idx int not null,
    card_id int not null,
    check (idx >= 0 and idx < 12),
    check (card_id >= 0 and card_id <= 81)
);

create unique index idx_0005 on board_cards using btree (game_id,idx);
create unique index idx_0006 on board_cards using btree (game_id,card_id)
    where card_id < 81; -- card 81 is empty


-- moves table

drop table if exists moves cascade;

create table moves (
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    player_id bigint not null references players (id)
        on delete cascade,
    idx0 int not null,
    card0_id int not null,
    idx1 int not null,
    card1_id int not null,
    idx2 int not null,
    card2_id int not null,
    created_at timestamp without time zone not null default now()
);

create index idx_0007 on moves using btree (game_id,player_id);
create index idx_0008 on moves using btree (game_id,created_at);

-- shuffles table

drop table if exists shuffles cascade;

create table shuffles (
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    player_id bigint not null references players (id)
        on delete cascade,
    sets_on_board int not null default 0,
    created_at timestamp without time zone not null default now()
);

create index idx_0009 on shuffles using btree (game_id,player_id);

--
-- Setup data triggers
--


-- base 36, min, max
-- boundary is 75% of unique values + min

-- 2-char
-- 10    36
-- zz 1_295
--      980 boundary

-- 3-char
-- 100  1_296
-- zzz 46_655
--     35_315 boundary

-- 4-char
-- 1000    46_656
-- zzzz 1_679_615
--      1_271_375 boundary

-- 5-char
-- 10000  1_679_616
-- zzzzz 60_466_175
--       45_769_535 boundary

-- 6-char
-- 100000    60_466_176
-- zzzzzz 2_176_782_335
--        1_647_703_295 boundary
drop function if exists set_game_id cascade;
CREATE OR REPLACE FUNCTION set_game_id() RETURNS trigger AS $$
DECLARE
    game_id bigint;
    next_id_counter bigint;
    min_value bigint;
    max_value bigint;
    done bool;
BEGIN
    next_id_counter := nextval('games_id_counter_seq'::regclass);
    NEW.id_counter = next_id_counter;
    if NEW.id is null then
        if next_id_counter <= 980 then -- 2-char
            min_value := 36;
            max_value := 1295;
        elsif next_id_counter <= 35315 then -- 3-char
            min_value := 1296;
            max_value := 46655;
        elsif next_id_counter <= 1271375 then -- 4-char
            min_value := 46656;
            max_value := 1679615;
        elsif next_id_counter <= 45769535 then -- 5-char
            min_value := 1679616;
            max_value := 60466175;
        else -- 6-char
            min_value := 60466176;
            max_value := 2176782335;
        end if;
        done := false;
        while not done loop
            game_id := floor(random()*(max_value-min_value+1))+min_value;
            done := not exists(select 1 from games where id=game_id);
        end loop;
        NEW.id = game_id;
    end if;
    return NEW;
END;
$$ language plpgsql;

drop trigger if exists set_game_id_trigger
    on games;
create trigger set_game_id_trigger
    before insert
    on games
    for each row execute procedure set_game_id();

--
-- Setup notification triggers
--

-- games_update notification

create or replace function games_update_notify() returns trigger AS $$
begin
    perform pg_notify(concat('game_', NEW.id), json_build_object(
        'type', 'game_update',
        'card_idx', NEW.card_idx,
        'status', NEW.status
    )::text);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists games_update_trigger on games;
create trigger games_update_trigger
    after update
    on games
    for each row execute procedure games_update_notify();

-- games data notification

create or replace function games_data_notify() returns trigger AS $$
declare
    msg text;
begin
    if NEW.presence then
        select
            row_to_json(data)::text into msg
        from (
            select
            'game_data' as type,
            (
                select json_agg(p)
                from (
                    select
                        gp.player_id,
                        p.name,
                        gp.presence,
                        (
                            select count(*)
                            from moves
                            where moves.game_id = gp.game_id
                            and moves.player_id = gp.player_id
                        ) as score,
                        (
                            select count(*)
                            from shuffles
                            where shuffles.game_id = gp.game_id
                            and shuffles.player_id = gp.player_id
                        ) as shuffles
                    from games_players gp
                    inner join players p
                    on gp.player_id = p.id
                    where gp.game_id = NEW.game_id
                    order by gp.created_at asc
                ) p
            ) as player_data,
            (
                select json_agg(b)
                from (
                    select
                        'board_card' as type,
                        idx,
                        card_id
                    from board_cards
                    where game_id = NEW.game_id
                    order by idx asc
                ) b
            ) as board_data,
            (
                select json_build_object(
                    'type', 'game_update',
                    'card_idx', card_idx,
                    'status', status
                )
                from games
                where id = NEW.game_id
            ) as game_update
        ) data;
    else
        msg := json_build_object(
            'type', 'presence',
            'player_id', NEW.player_id,
            'presence', NEW.presence
        )::text;
    end if;

    perform pg_notify(concat('game_', NEW.game_id), msg);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists games_data_trigger on games_players;
create trigger games_data_trigger
    after insert or update
    on games_players
    for each row execute procedure games_data_notify();


-- players_name notification

create or replace function players_name_change_notify() returns trigger AS $$
declare
    game_id bigint;
begin
    for game_id in
        select gp.game_id
        from games_players gp
        where player_id = NEW.id
        and presence = 't'
    loop
        perform pg_notify(concat('game_', game_id), json_build_object(
            'type', 'player_name',
            'player_id', NEW.id,
            'name', NEW.name
        )::text);
    end loop;
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists players_name_change_trigger
    on players;
create trigger players_name_change_trigger
    after update
    on players
    for each row execute procedure players_name_change_notify();

-- board_card notification

create or replace function board_card_change_notify() returns trigger AS $$
begin
    perform pg_notify(concat('game_', NEW.game_id), json_build_object(
        'type', 'board_card',
        'idx', NEW.idx,
        'card_id', NEW.card_id
    )::text);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists board_card_change_trigger
    on board_cards;
create trigger board_card_change_trigger
    after update
    on board_cards
    for each row execute procedure board_card_change_notify();

-- moves created notification

create or replace function moves_insert_notify() returns trigger AS $$
declare
    score integer;
    msg text;
begin
    select count(*) into score
    from moves
    where moves.game_id = NEW.game_id
    and moves.player_id = NEW.player_id;

    select
        row_to_json(data)::text into msg
    from (
        select
        'move_data' as type,
        (
            select json_build_object(
                'type', 'score',
                'player_id', NEW.player_id,
                'score', score
            )
        ) as score,
        (
            select json_build_object(
                'type', 'previous_move',
                'card0_id', NEW.card0_id,
                'card1_id', NEW.card1_id,
                'card2_id', NEW.card2_id
            )
        ) as previous_move
    ) as data;

    perform pg_notify(concat('game_', NEW.game_id), msg);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists moves_insert_trigger on moves;
create trigger moves_insert_trigger
    after insert
    on moves
    for each row execute procedure moves_insert_notify();

-- shuffles created notification

create or replace function shuffles_insert_notify() returns trigger AS $$
declare
    shuffles integer;
    msg text;
begin
    select count(*) into shuffles
    from shuffles
    where shuffles.game_id = NEW.game_id
    and shuffles.player_id = NEW.player_id;

    perform pg_notify(concat('game_', NEW.game_id), json_build_object(
        'type', 'shuffles',
        'player_id', NEW.player_id,
        'shuffles', shuffles
    )::text);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists shuffles_insert_trigger on shuffles;
create trigger shuffles_insert_trigger
    after insert
    on shuffles
    for each row execute procedure shuffles_insert_notify();
