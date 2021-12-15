% Nombre: Christopher Alejandro Torres Aceituno
% Rut: 21.024.191-4
% Proyecto de laboratorio N°2
% Plataforma que emula Google Docs

% escribircomando/ ;true. enter w a
%-----------%
% TDA Fecha %
%-----------%
%
% Dominios
%    D: un número entero (representa el día)
%    M: un número entero (representa el mes)
%    A: un número entero (representa el año)
%
% Predicados
%    fecha(D, M, A, FOut). aridad = 4 
%
% Clausulas
% Hechos
mes(1,31).
mes(2,28).
mes(2,29).
mes(3,31).
mes(4,30).
mes(5,31).
mes(6,30).
mes(7,31).
mes(8,31).
mes(9,30).
mes(10,31).
mes(11,30).
mes(12,31).

% Reglas
% Constructor
fecha(D, M, A, Fecha):-
    integer(D), integer(M), integer(A), mes(M, Dias), D>0, D=<Dias, Fecha = [D, M, A].

% Pertinencia
esFecha(Fecha):-
    is_list(Fecha).

% Selectores
seleccionarDiaF([D, _, _], D).
seleccionarMesF([_, M, _], M).
seleccionarAnnoF([_, _, A], A).

% Modificadores
modificarDiaF(F1, D, F2):-
    seleccionarMesF(F1, M1), seleccionarAnnoF(F1, A1), fecha(D, M1, A1, F2), esFecha(F2).
modificarMesF(F1, M, F2):-
    seleccionarDiaF(F1, D1), seleccionarAnnoF(F1, A1), fecha(D1, M, A1, F2), esFecha(F2).
modificarAnnoF(F1, A, F2):-
    seleccionarDiaF(F1, D1), seleccionarMesF(F1, M1), fecha(D1, M1, A, F2), esFecha(F2).


%-------------------%
% TDA ParadigmaDocs %
%-------------------%
%
% Dominios
%    Name: String (Nombre de la plataforma)
%    Date: Fecha (Fecha de creación)
%
% Predicados
%    paradigmaDocs(Name, Date, POut). aridad = 3
%     
% Clausulas
% Constructor/Pertinencia/Modificadores
paradigmaDocs(Name, Date, ParadigmaDocs):-
    string(Name), esFecha(Date), ParadigmaDocs = [Name, Date, [], [], []].
% Ejemplo: fecha(14, 12, 2021, F1), paradigmaDocs("Plataforma1", F1, Gdocs000).

% Selectores
selectNombreP([NombreP, _, _, _, _], NombreP).
selectFechaP([_, FechaP, _, _, _], FechaP).
selectListaRegP([_, _, ListaReg, _, _], ListaReg).
selectUserActivoP([_, _, _, UserActivo, _], UserActivo).
selectDocumentosP([_, _, _, _, ListaDocs], ListaDocs).

%-------------%
% TDA Usuario %
%-------------%
%
% Dominios
%    Username: String (Nombre de usuario)
%    Password: String (Contraseña del usuario)
%    Date: Fecha (Fecha de creación)
%
% Predicados
%    usuario(Username, Password, Date, UOut) aridad = 4
%     
% Clausulas
% Constructor/Pertinencia/Selector/Modificadores
usuario(Username, Password, Date, User):-
    string(Username), string(Password), esFecha(Date), User = [Username, Password, Date].
% Ejemplo: fecha(14, 12, 2021, F1), usuario("Grillitoxc", "123", F1, User1).


%---------------------------------------------------%
% Código Principal Plataforma que emula Google Docs %
%---------------------------------------------------%
%

%---paradigmaDocsRegister---%
paradigmaDocsRegister(PD1, Fecha, Username, Password, PD2):-
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    indexOf(ListaReg, )
    usuario(Username, Password, Fecha, User1),
    append(ListaReg, [User1], ListaRegistroNueva),
    PD2 = [NombreP, FechaP, ListaRegistroNueva, UserActivo, ListaDocs].


% fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4).
% fecha(14, 12, 2021, F1), paradigmaDocs("Plataforma1", F1, Gdocs000), paradigmaDocsRegister(Gdocs000, F1, "Griyitoxc", "123", Gdocs001).



