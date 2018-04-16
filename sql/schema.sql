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
    game_id integer not null references games (id)
        on delete cascade,
    player_id integer not null references players (id)
        on delete cascade,
    present boolean default true,
    created_at timestamp without time zone not null default now(),
    updated_at timestamp without time zone not null default now(),
    primary key (game_id, player_id)
);

create index idx_0000 on games_players using btree (game_id);
create index idx_0001 on games_players using btree (player_id);

create or replace function games_players_present_change_notify() returns trigger AS $$
begin
    perform pg_notify(concat('game_', NEW.game_id), json_build_object(
        'type', 'present',
        'game_id', NEW.game_id,
        'player_id', NEW.player_id,
        'value', NEW.present
    )::text);
    return NEW;
end;
$$ language plpgsql;

drop trigger if exists games_players_present_change_trigger
    on games_players;
create trigger games_players_present_change_trigger
    after insert or update
    on games_players
    for each row execute procedure games_players_present_change_notify();

drop table if exists game_cards cascade;

create table game_cards (
    id serial not null primary key,
    game_id integer not null references games (id)
        on delete cascade,
    idx integer not null,
    card_id integer not null
);

create unique index idx_0002 on game_cards using btree (game_id,idx);
create unique index idx_0003 on game_cards using btree (game_id,card_id);
