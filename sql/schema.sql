drop table if exists players cascade;

create table players (
    id serial not null primary key,
    name character varying(255) not null default '',
    created_at timestamp without time zone default now()
);

--  1_679_616 is 5-char base-36 min (10000)
-- 60_466_175 is 5-char base-36 max (zzzzz)
-- 58_786_559 unique values
-- formula is SELECT floor(random()*(max-min+1))+min;
CREATE OR REPLACE FUNCTION make_game_id() RETURNS integer AS $$
DECLARE
    new_game_id integer;
    done bool;
BEGIN
    done := false;
    WHILE NOT done LOOP
        new_game_id := floor(random()*(58786560))+1679616;
        done := NOT exists(SELECT 1 FROM games WHERE id=new_game_id);
    END LOOP;
    RETURN new_game_id;
END;
$$ LANGUAGE PLPGSQL VOLATILE;

drop table if exists games cascade;

create table games (
    id integer not null primary key default make_game_id(),
    created_at timestamp without time zone default now(),
    check (id >= 1679616 and id <= 60466175)
);

drop table if exists games_players cascade;

create table games_players (
    game_id integer not null references games (id),
    player_id integer not null references players (id),
    present boolean default true,
    created_at timestamp without time zone default now(),
    primary key (game_id, player_id)
);