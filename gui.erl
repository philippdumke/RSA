-module(gui).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start() ->
    State = make_window(),
    loop(State).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Krypto", [{size,{800, 965}}]),
    Panel  = wxPanel:new(Frame),

%% Erstellen der Sizer für das Layout
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer1 = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[{label, "Eingabe"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Sizer3 = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Privat"}]),
    Sizer4 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[]),
    SizerH1 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel, []),
    SizerV1 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    SizerH2 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel, []),
    SizerV2 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    SizerH3 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel, []),
    SizerV3 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    Sizer5 = wxStaticBoxSizer:new(?wxHORIZONTAL,Panel,[{label, "Status"}]),
    SizerH4 = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[]),
    SizerV4 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    SizerHash = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[]),
    Sizer6 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[{label, "Öffentlich"}]),
    Sizer7 = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[]),
    Sizer8 = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,[]),
    SizerV7 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    SizerV8 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),
    SizerV9 = wxStaticBoxSizer:new(?wxVERTICAL,Panel,[]),



%% Erstellen von Widgets
    T1001 = wxTextCtrl:new(Panel, 1001,[{value, "lorem ipsum dolor"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1002 = wxTextCtrl:new(Panel, 1002, [{value," "},{style,?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    T1003 = wxTextCtrl:new(Panel, 1003, [{value,"Öffentlicher Modul"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1004 = wxTextCtrl:new(Panel, 1004, [{value,"Öffentlicher Exponent"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1005 = wxTextCtrl:new(Panel, 1005,[{value,"ok."},{style,?wxDEFAULT bor ?wxTE_READONLY}]),
    T1006 = wxTextCtrl:new(Panel, 1006,[{value, "5"},{style,?wxDEFAULT}]),
    T1007 = wxTextCtrl:new(Panel, 1007,[{value, "Privater Exponent"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1008 = wxTextCtrl:new(Panel, 1008,[{value, "49755721138930176511"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1009 = wxTextCtrl:new(Panel, 1009,[{value, "546509248236243873484442814273"},{style,?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1010 = wxTextCtrl:new(Panel, 1010,[{value, "Signatur "},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    T1011 = wxTextCtrl:new(Panel,1011,[{value,"154"},{style, ?wxDEFAULT}]),
    B101  = wxButton:new(Panel, 101, [{label, "Verschlüsseln"}]),
    B102  = wxButton:new(Panel, 102, [{label,"Entschlüsseln"}]),
    B103  = wxButton:new(Panel, 103, [{label, "KeyGen"}]),
    B104  = wxButton:new(Panel, 104,[{label, "Test"}]),
    ST2001 = wxStaticText:new(Panel, 2001,"", []),
    ST2002 = wxStaticText:new(Panel, 2002,"Ergebnis ", []),
    ST2003 = wxStaticText:new(Panel, 2003,"", []),
    ST2004 = wxStaticText:new(Panel, 2004,"", []),
    ST2005 = wxStaticText:new(Panel, 2005," Modul    ", []),
    ST2006 = wxStaticText:new(Panel, 2006,"", []),
    ST2007 = wxStaticText:new(Panel, 2007,"", []),
    ST2008 = wxStaticText:new(Panel, 2008,"Exponent", []),
    ST2009 = wxStaticText:new(Panel, 2009,"", []),
    ST2010 = wxStaticText:new(Panel, 2010,"Blocklänge",[]),
    ST2011 = wxStaticText:new(Panel, 2011,"",[]),
    ST2012 = wxStaticText:new(Panel, 2012,"Priv. Exp.",[]),
    ST2013 = wxStaticText:new(Panel, 2013,"",[]),
    ST2014 = wxStaticText:new(Panel, 2014,"Signatur: ",[]),
    ST2015 = wxStaticText:new(Panel, 2015, "",[]), %% Für Signatur
    ST2016 = wxStaticText:new(Panel, 2016, " Modul    ", []),
    ST2017 = wxStaticText:new(Panel, 2017, "Exponent", []),
    ST2018 = wxStaticText:new(Panel, 2018,"",[]),
    ST2019 = wxStaticText:new(Panel, 2019,"",[]),
    ST2020 = wxStaticText:new(Panel, 2020,"",[]),
    ST2021 = wxStaticText:new(Panel, 2021,"",[]),
    ST2022 = wxStaticText:new(Panel, 2022,"Schlüssellänge",[]),


    %% Plazieren der Eingabe, sowie der beiden Buttons
    wxSizer:add(Sizer1,T1001,[{flag,?wxEXPAND}, {proportion,1}]),
    wxSizer:add(Sizer2,B101,[{flag,?wxEXPAND}]),
    wxSizer:add(Sizer2,B102,[{flag,?wxEXPAND}]),
    wxSizer:add(Sizer2,B103,[{flag,?wxEXPAND}]),
    wxSizer:add(Sizer2,B104,[{flag,?wxEXPAND}]),
    wxSizer:add(Sizer1,Sizer2,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer, Sizer1,[{flag, ?wxEXPAND}]),
    %% Sizer Hash
    wxSizer:add(SizerV9,ST2014,[]),
    wxSizer:add(SizerV9,ST2015,[]),
    wxSizer:add(SizerHash,SizerV9,[]),
    wxSizer:add(SizerHash,T1010,[{flag, ?wxEXPAND},{proportion,1}]),
    wxSizer:add(MainSizer,SizerHash,[{flag, ?wxEXPAND}]),

    %Blocklänge
    wxSizer:add(Sizer4, ST2010,[]),
    wxSizer:add(Sizer4, T1006,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer4, ST2022,[]),
    wxSizer:add(Sizer4, T1011,[{flag,?wxEXPAND},{proportion,1}]),

    %% Ergebnis
    wxSizer:add(SizerV1,ST2001, []),
    wxSizer:add(SizerV1,ST2002,[]),
    wxSizer:add(SizerV1,ST2003,[]),
    wxSizer:add(SizerH1, SizerV1,[]),
    wxSizer:add(SizerH1, T1002,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer3, SizerH1,[{flag,?wxEXPAND}]),

    %% Modul
    wxSizer:add(SizerV2,ST2004, []),
    wxSizer:add(SizerV2,ST2005, []),
    wxSizer:add(SizerV2,ST2006, []),
    wxSizer:add(SizerH2,SizerV2,[]),
    wxSizer:add(SizerH2, T1003,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer3,SizerH2,[{flag,?wxEXPAND}]),

    %% Exponent
    wxSizer:add(SizerV3,ST2007, []),
    wxSizer:add(SizerV3,ST2008, []),
    wxSizer:add(SizerV3,ST2009, []),
    wxSizer:add(SizerH3,SizerV3, []),
    wxSizer:add(SizerH3,T1004,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer3,SizerH3,[{flag,?wxEXPAND}]),

    %% Privater Exponent
    wxSizer:add(SizerV4,ST2011,[]),
    wxSizer:add(SizerV4,ST2012,[]),
    wxSizer:add(SizerV4,ST2013,[]),
    wxSizer:add(SizerH4,SizerV4,[]),
    wxSizer:add(SizerH4,T1007,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer3,SizerH4,[{flag,?wxEXPAND}]),

    wxSizer:add(Sizer5,T1005, [{flag,?wxEXPAND},{proportion,1}]),
    %% Sizer zum Frame hinzugüfgen
    wxSizer:add(MainSizer,Sizer4,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,Sizer3,[{flag,?wxEXPAND}]),


    wxSizer:add(SizerV7,ST2018,[]),
    wxSizer:add(SizerV7,ST2016,[]),
    wxSizer:add(SizerV7,ST2019,[]),

    wxSizer:add(SizerV8,ST2020,[]),
    wxSizer:add(SizerV8,ST2017,[]),
    wxSizer:add(SizerV8,ST2021,[]),
    wxSizer:add(Sizer7,SizerV7,[]),
    wxSizer:add(Sizer7,T1008,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer8,SizerV8,[]),
    wxSizer:add(Sizer8,T1009,[{flag,?wxEXPAND},{proportion,1}]),
    wxSizer:add(Sizer6,Sizer7,[{flag,?wxEXPAND}]),
    wxSizer:add(Sizer6,Sizer8,[{flag,?wxEXPAND}]),
    wxSizer:add(MainSizer,Sizer6,[{flag,?wxEXPAND}]),


    wxSizer:add(MainSizer,Sizer5,[{flag,?wxEXPAND}]),


    wxPanel:setSizer(Panel,MainSizer),
    %% Frame anzeigen
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_button_clicked),
    {Frame, T1001, T1002, T1003, T1004, T1005, T1006, T1007, T1008, T1009, T1010, T1011}.

loop(State) ->
    {Frame, T1001, T1002, T1003, T1004, T1005,T1006, T1007, T1008, T1009, T1010, T1011}  = State,
    receive
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Frame),
            io:format("Exit Button klicked\n",[]),
            ok;
        %% Button verschlüsseln
        #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
            io:format("Button Verschlüsseln was klicked",[]),
	    io:format(" ~p \n",[wxTextCtrl:getValue(T1001)]),
	    wxTextCtrl:changeValue(T1002, wxTextCtrl:getValue(T1001)),
            LEN = length(wxTextCtrl:getValue(T1001)),
            if LEN > 0 ->
                    %% Konkatenieren der Eingabefelder um mit einem regulären Ausdruck alle Felder auf falsche Eingaben zu testen
                    Match = wxTextCtrl:getValue(T1003) ++ wxTextCtrl:getValue(T1004) ++ wxTextCtrl:getValue(T1006) ++ wxTextCtrl:getValue(T1007) ++ wxTextCtrl:getValue(T1008) ++ wxTextCtrl:getValue(T1009),

                    %% Prüft ob sich in den Feldern Blocklänge, Priv.Exp., öffentlicher Modul, Exponent Buchstaben befinden
                    %% Wird kein Treffer gefunden "nomatch" fährt das Programm mit dem Verschlüsseln fort
                    %% Wird ein Treffer gefunden, wird ein entsprechender Kommentar in der StatusLeiste angezeigt
                    case re:run(Match,"[^0-9]") of
                        nomatch ->
                                    case list_to_integer(wxTextCtrl:getValue(T1006)) > 0 of
                                           true  ->io:format("",[]); % verschluesseln_proxy(State),
                                                    %gen_sig(State);
                                           false -> wxTextCtrl:changeValue(T1005,"error Blocklänge muss größer als Null sein.")
                                    end;
                        {match,_}  ->  wxTextCtrl:changeValue(T1005, "error Falsche Eingabe.")
                    end;
            true -> wxTextCtrl:changeValue(T1005,"error leere Eingabe.")
            end,

            loop(State);


        %% Button entschlüsseln
        #wx{id = 102, event=#wxCommand{type = command_button_clicked}} ->
            io:format("Button Entschlüsseln was klicked",[]),
            %% Konkateeniert Modul und priv.Exp.
            Match = wxTextCtrl:getValue(T1003) ++ wxTextCtrl:getValue(T1004) ++ wxTextCtrl:getValue(T1006) ++ wxTextCtrl:getValue(T1007) ++ wxTextCtrl:getValue(T1008) ++ wxTextCtrl:getValue(T1009),

            LEN = length(wxTextCtrl:getValue(T1001)),
            if LEN > 0 ->
                    case re:run(Match,"[^0-9]") of
                        nomatch ->
                                    case list_to_integer(wxTextCtrl:getValue(T1006)) > 0 of
                                            true  -> entschluesseln_proxy(State),
                                                     ck_sig(State);
                                            false -> wxTextCtrl:changeValue(T1005, "error Blocklänge muss größer als Null sein.")
                                    end;
                        {match,_} -> wxTextCtrl:changeValue(T1005,"error Falsche Eingabe.")
                    end;
            true -> wxTextCtrl:changeValue(T1005,"error leere Eingabe")
            end,
            loop(State);


        %% Generiere Keys
        #wx{id =103, event=#wxCommand{type =command_button_clicked}} ->
            io:format("Button Gen Key was klicked" ,[]),

            case re:run(wxTextCtrl:getValue(T1011),"[^0-9]") of
                nomatch ->
                            %% Abfrage der Blocklänge aus der GUI
                            T1011_val = list_to_integer(wxTextCtrl:getValue(T1011)),
                            if T1011_val > 0 ->
                                    %% Setzen des Status
                                    wxTextCtrl:changeValue(T1005, "Berechne Primzahlen, dies dauert einen Moment ..."),
                                    %% Auftuf der compute_prime Funktion. Diese generiert zwei Primzahlen A,B und gibt diese als Tupel zurück
                                    {A,B} = krypto:compute_primes(T1011_val),
                                     %% Statusinformationen auf der Commandline
                                    io:format("Primzahlen ~p \n",[A]),
                                    io:format("Primzahlen ~p \n",[B]),
                                    %% Setzen des Moduls
                                    wxTextCtrl:changeValue(T1003, integer_to_list(A * B)),
                                    %wxTextCtrl:changeValue(T1008,integer_to_list(A*B)),   %% Test
                                    M = ((A-1)*(B-1)),
                                    wxTextCtrl:changeValue(T1005, "Berechne eine relativ prime Zahl"),
                                    %% Erzeuge eine relativ prime Zahl
                                    Exp = krypto:make_rel_prime(krypto:make(50),M),
                                    io:format("Exp: ~p \n",[Exp]),
                                    wxTextCtrl:changeValue(T1004, integer_to_list(Exp)),
                                    wxTextCtrl:changeValue(T1005, "Berechne das Multiplikativ Inverse"),
                                    %% Bestimmen des privaten Schlüssels
                                    wxTextCtrl:changeValue(T1007, integer_to_list(krypto:multiplikativInverses(Exp,M))),
                                    wxTextCtrl:changeValue(T1005, "ok.");
                               true -> wxTextCtrl:changeValue(T1005,"error Die Länge der Primzahlen muss größer sein.")
                            end;
                {match,_} ->
                            wxTextCtrl:changeValue(T1005, "error Schlüssellänge muss nummerisch sein.")
            end,
            loop(State);


        #wx{id=104, event=#wxCommand{type = command_button_clicked}} ->
            io:format("Button Test was klicked",[]),
            LEN = length(wxTextCtrl:getValue(T1001)),
            if LEN > 0 ->
                    Match = wxTextCtrl:getValue(T1003) ++ wxTextCtrl:getValue(T1004) ++ wxTextCtrl:getValue(T1006) ++ wxTextCtrl:getValue(T1007) ++ wxTextCtrl:getValue(T1008) ++ wxTextCtrl:getValue(T1009),
                    case re:run(Match,"[^0-9]") of
                        nomatch ->
                                    %%  kopieren des Moduls
                                    wxTextCtrl:changeValue(T1008,wxTextCtrl:getValue(T1003)),
                                    %% Kopieren des Exponenten
                                    wxTextCtrl:changeValue(T1009, wxTextCtrl:getValue(T1004)),

                                    %% Speichern der Eingabe
                                    Input = string:strip(wxTextCtrl:getValue(T1001),right),
                                    %% Verschlüsseln
                                    verschluesseln_proxy(State),
                                    %% Signatur erstellen
                                    gen_sig(State),
                                    %% Verschlüsselten Text in die Eingabe Kopieren
                                    wxTextCtrl:changeValue(T1001, wxTextCtrl:getValue(T1002)),

                                    %% Entschlüsselna
                                    entschluesseln_proxy(State),
                                    %% Ergebnis mit der Eingabe prüfen
                                    case string:strip(wxTextCtrl:getValue(T1002),right) of
                                        Input -> wxTextCtrl:changeValue(T1005, "Ein- und Ausgabe stimmen überein.   ok.");
                                        true -> wxTextCtrl:changeValue(T1005, "Ein und Ausgabe stimmen nicht überein. error.")
                                    end,
                                    ck_sig(State);
                        {match,_} ->
                                    wxTextCtrl:changeValue(T1005, "error Falsche Eingabe")
                    end;
            true -> wxTextCtrl:changeValue(T1005,"error leere Eingabe")
            end,
            loop(State);

            %% Status in der Statusleiste ausgeben

        Msg ->
           %everything else
           % ...i
           io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
           loop(State)
    end.


verschluesseln_proxy(State) ->
            {Frame, T1001, T1002, T1003, T1004, T1005, T1006, T1007, T1008, T1009, T1010, T1011}  = State,
            %% Übergabe der einzelnen Textfelder an entsprechende Variablen als Integer
            T1001_val = wxTextCtrl:getValue(T1001), %Input
            T1006_val = list_to_integer(wxTextCtrl:getValue(T1006)),% Blocklänge
            T1008_val = list_to_integer(wxTextCtrl:getValue(T1008)), % entfernter Modul
            T1009_val = list_to_integer(wxTextCtrl:getValue(T1009)),% entfernter exponent
            MapLen = length(maps:to_list(krypto:build_map(emap))),
            %% Prüft ob die Lenge der Map  hoch Eingtragene Blocklänge kleiner als der Modul ist
            XY = krypto:fastExponentiation(MapLen,T1006_val) < T1008_val,
            if XY  ->
                       %% Ändert den Wert fpr den Verschlüsselten String mit dem Ergebnis der Funktion verschlüsseln
                       wxTextCtrl:changeValue(T1002,krypto:verschluesseln(T1001_val, T1006_val,T1009_val, T1008_val)),
                       %% Setze Status
                       wxTextCtrl:changeValue(T1005, "ok.");
               true -> wxTextCtrl:changeValue(T1005, "Blocklänge muss kleinere gewählt werden"),
                       loop(State)
            end.

entschluesseln_proxy(State) ->
            {Frame, T1001, T1002, T1003, T1004, T1005, T1006, T1007, T1008, T1009, T1010, T1011}  = State,
            T1001_val = wxTextCtrl:getValue(T1001), % Eingabe
            T1006_val = list_to_integer(wxTextCtrl:getValue(T1006)), % Blocklänge
            T1003_val = list_to_integer(wxTextCtrl:getValue(T1003)), % Eigener Modul
            T1007_val = list_to_integer(wxTextCtrl:getValue(T1007)), % privater Exponent

            MapLen = length(maps:to_list(krypto:build_map(emap))),
            %% Prüfen ob Maplänge hoch Blocklänge kleiner ist als der Modul
            XY = krypto:fastExponentiation(MapLen,T1006_val) < T1003_val,
            BlockL = krypto:calc_exponent(T1003_val,length(maps:to_list(krypto:build_map(emap))),T1006_val),
            if XY  ->
                %% Funktion entschlüsseln
                Result = krypto:entschluesseln(T1001_val,BlockL,T1007_val,T1003_val,T1006_val),
                wxTextCtrl:changeValue(T1002,Result),    %% Update Text in wxTextCtrl
                %% Setzen des Status
                wxTextCtrl:changeValue(T1005, "ok.");
              true -> wxTextCtrl:changeValue(T1005, "Blocklänge muss kleinere gewählt werden"),
                      loop(State)
            end.

gen_sig(State) ->
    {Frame, T1001, T1002, T1003, T1004, T1005, T1006, T1007, T1008, T1009, T1010, T1011}  = State,

    Input = wxTextCtrl:getValue(T1001),
    Modul = list_to_integer(wxTextCtrl:getValue(T1003)),
    Exp = list_to_integer(wxTextCtrl:getValue(T1007)),
    Chiffre = md5:md5_hex(Input),
    wxTextCtrl:changeValue(T1010,krypto:verschluesseln(Chiffre,50,Exp,Modul)).

ck_sig(State) ->
    {Frame, T1001, T1002, T1003, T1004, T1005, T1006, T1007, T1008, T1009, T1010, T1011}  = State,

    Input = wxTextCtrl:getValue(T1002),
    CSig = wxTextCtrl:getValue(T1010),
    Modul = list_to_integer(wxTextCtrl:getValue(T1008)),
    Exp = list_to_integer(wxTextCtrl:getValue(T1009)),
    BlockL = krypto:calc_exponent(Modul,length(maps:to_list(krypto:build_map(emap))),50),
    Sig = string:strip(krypto:entschluesseln(CSig,BlockL,Exp,Modul,50),right),
    io:format("entschlüsselt: ~p",[Sig]),
    Hash = md5:md5_hex(string:strip(Input,right)),
    io:format("hash: ~p",[Hash]),

    if Sig =:= Hash -> wxTextCtrl:changeValue(T1005, "Signatur ok.");
        true -> wxTextCtrl:changeValue(T1005,"Signatur stimmt nicht überein")
    end.









