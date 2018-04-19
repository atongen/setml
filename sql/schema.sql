drop table if exists players cascade;

create table players (
    id bigserial not null primary key,
    name character varying(255) not null default '',
    created_at timestamp without time zone default now()
);

drop function if exists make_game_id cascade;

--  1_679_616 is 5-char base-36 min (10000)
-- 60_466_175 is 5-char base-36 max (zzzzz)
-- 58_786_559 unique values
-- formula is SELECT floor(random()*(max-min+1))+min;
CREATE OR REPLACE FUNCTION make_game_id() RETURNS bigint AS $$
DECLARE
    new_game_id bigint;
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
    id bigint not null primary key default make_game_id(),
    card_idx int not null default 0,
    created_at timestamp without time zone default now(),
    check (id >= 1679616 and id <= 60466175)
);

drop table if exists games_players cascade;

create table games_players (
    game_id bigint not null references games (id)
        on delete cascade,
    player_id bigint not null references players (id)
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
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    idx int not null,
    card_id int not null,
    check (idx >= 0 and idx < 81),
    check (card_id >= 0 and card_id < 81)
);

create unique index idx_0002 on game_cards using btree (game_id,idx);
create unique index idx_0003 on game_cards using btree (game_id,card_id);

drop table if exists board_cards cascade;

create table board_cards (
    id bigserial not null primary key,
    game_id bigint not null references games (id)
        on delete cascade,
    idx int not null,
    card_id bigint not null,
    check (idx >= 0 and idx < 12),
    check (card_id >= 0 and card_id < 81)
);

create unique index idx_0004 on board_cards using btree (game_id,idx);
create unique index idx_0005 on board_cards using btree (game_id,card_id);

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

create index idx_0006 on moves using btree (game_id,player_id);
create index idx_0007 on moves using btree (game_id,created_at);
