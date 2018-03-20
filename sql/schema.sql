drop table if exists players cascade;

create table players (
    id serial not null primary key,
    name character varying(255) not null default '',
    created_at timestamp without time zone default now()
);

CREATE OR REPLACE FUNCTION make_game_id() RETURNS integer AS $$
DECLARE
    new_game_id integer;
    done bool;
BEGIN
    done := false;
    WHILE NOT done LOOP
        new_game_id := floor(random()*(60466176)); -- 60,466,175 is max in 5 character base-36
        done := NOT exists(SELECT 1 FROM games WHERE id=new_game_id);
    END LOOP;
    RETURN new_game_id;
END;
$$ LANGUAGE PLPGSQL VOLATILE;

drop table if exists games cascade;

create table games (
    id integer not null primary key default make_game_id(),
    created_at timestamp without time zone default now()
);

drop table if exists games_players cascade;

create table games_players (
    game_id integer not null references games (id),
    player_id integer not null references players (id),
    present boolean default true,
    created_at timestamp without time zone default now(),
    primary key (game_id, player_id)
);