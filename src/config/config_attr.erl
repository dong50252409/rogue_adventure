%%===================================
%%  AUTO GENERATE BY CFG_EXPORTER
%%===================================
-module(config_attr).
-compile([export_all, nowarn_export_all]).
-include("config_attr.hrl").


get(2) ->
    #config_attr{
        id = 2,
        name = <<"罗格"/utf8>>,
        sign = <<"R"/utf8>>,
        sign_color = on_green,
        word_color = green,
        module = unit_rogue,
        atk = 1000,
        def = 1000,
        mov = 1500,
        spd = 1200,
        hp = 10000,
        cri = 50,
        guard_range = 3,
        atk_range = 3,
        args = undefined
    };
get(3) ->
    #config_attr{
        id = 3,
        name = <<"恶蛆"/utf8>>,
        sign = <<"M"/utf8>>,
        sign_color = on_blue,
        word_color = blue,
        module = unit_maggot,
        atk = 800,
        def = 500,
        mov = 1200,
        spd = 800,
        hp = 2500,
        cri = 10,
        guard_range = 2,
        atk_range = 1,
        args = 30
    };
get(4) ->
    #config_attr{
        id = 4,
        name = <<"蜈蚣"/utf8>>,
        sign = <<"C"/utf8>>,
        sign_color = on_cyan,
        word_color = cyan,
        module = unit_centipede,
        atk = 850,
        def = 700,
        mov = 1800,
        spd = 1300,
        hp = 6000,
        cri = 10,
        guard_range = 3,
        atk_range = 2,
        args = 30
    };
get(5) ->
    #config_attr{
        id = 5,
        name = <<"毒蝎"/utf8>>,
        sign = <<"S"/utf8>>,
        sign_color = on_magenta,
        word_color = magenta,
        module = unit_scorpion,
        atk = 850,
        def = 1000,
        mov = 1000,
        spd = 900,
        hp = 9000,
        cri = 5,
        guard_range = 5,
        atk_range = 2,
        args = 20
    };
get(6) ->
    #config_attr{
        id = 6,
        name = <<"火蚁"/utf8>>,
        sign = <<"A"/utf8>>,
        sign_color = on_red,
        word_color = red,
        module = unit_ant,
        atk = 750,
        def = 600,
        mov = 1300,
        spd = 1000,
        hp = 4000,
        cri = 80,
        guard_range = 4,
        atk_range = 4,
        args = 30
    };
get(7) ->
    #config_attr{
        id = 7,
        name = <<"宝箱"/utf8>>,
        sign = <<"B"/utf8>>,
        sign_color = on_yellow,
        word_color = yellow,
        module = unit_box,
        atk = 0,
        def = 100,
        mov = 0,
        spd = 0,
        hp = 10000,
        cri = 0,
        guard_range = 0,
        atk_range = 0,
        args = 20
    };
get(_) ->
    undefined.

list() ->
    [
        2,
        3,
        4,
        5,
        6,
        7
    ].

