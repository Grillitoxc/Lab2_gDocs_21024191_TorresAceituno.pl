% Nombre: Christopher Alejandro Torres Aceituno
% Rut: 21.024.191-4
% Proyecto de laboratorio N°2
% Plataforma que emula Google Docs

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
%    fecha(D, M, A, FOut).                                                  aridad = 4
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
%    paradigmaDocs(Name, Date, POut).                                       aridad = 3
%     
% Clausulas
% Constructor/Pertinencia/Modificadores
paradigmaDocs(Name, Date, ParadigmaDocs):-
    string(Name), esFecha(Date), ParadigmaDocs = [Name, Date, [], [], []].

% Selectores
selectNombreP([NombreP, _, _, _, _], NombreP).
selectFechaP([_, FechaP, _, _, _], FechaP).
selectListaRegP([_, _, ListaReg, _, _], ListaReg).
selectUserActivoP([_, _, _, UserActivo, _], UserActivo).
selectDocumentosP([_, _, _, _, ListaDocs], ListaDocs).
selectNombreUser([User], User).

% Modificadores 
setListaVacia(PD1, PD2):-
    selectNombreP(PD1, Nombre1),
    selectFechaP(PD1, Fecha1),
    selectListaRegP(PD1, Registro1),
    selectDocumentosP(PD1, Documento1),
    PD2 = [Nombre1, Fecha1, Registro1,[], Documento1].

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
%    usuario(Username, Password, Date, UOut)                                aridad = 4
%     
% Clausulas
% Constructor/Pertinencia/Selector/Modificadores
usuario(Username, Password, Date, User):-
    string(Username), string(Password), esFecha(Date), User = [Username, Password, Date].

%---------------%
% TDA Documento %
%---------------%
%
% Dominios
%    Nombre: String (Nombre del documento)
%    Fecha: Fecha (Fecha de creación)
%
% Predicados
%    documento(Nombre, Fecha, Contenido, DOut)                              aridad = 4
%
% Clausulas
% Constructor/Pertinencia/Selector
documento(Nombre, Fecha, Contenido, Doc):-
    string(Nombre), esFecha(Fecha), string(Contenido),
    Doc = [Nombre, Fecha, "", [[Contenido, Fecha, 0]], []].

% Selectores
selectNombreD([NombreD, _, _, _, _], NombreD).
selectFechaD([_, FechaD, _, _, _], FechaD).
selectIdD([_, _, IdD, _, _], IdD).
selectVersiones([_, _, _, ListaVer, _], ListaVer).
selectAccesos([_, _, _, _, ListaAccesos], ListaAccesos).

% Modificadores
setIdDoc(Doc1, Id, Doc2):-
    selectNombreD(Doc1, NombreDoc), selectFechaD(Doc1, FechaDoc), selectVersiones(Doc1, VerDoc), selectAccesos(Doc1, AccDoc),
    Doc2 = [NombreDoc, FechaDoc, Id, VerDoc, AccDoc].

%-------------------------------------------------------------------------------%
% Otros predicados (que ayudan al funcionamiento de los predicados principales) %
%-------------------------------------------------------------------------------%
miembro(X, [X|_]):-!.
    miembro(X, [_|T]):-miembro(X, T).

primero([H|_], H).
sacarNombres([], []):-!.
sacarNombres([H|T], [H1|T2]):-
    primero(H, H1),
    sacarNombres(T, T2).

primeroAndSegundo([Name, Pass|_], [Name, Pass]).
sacarNombresAndContrasenas([], []):-!.
sacarNombresAndContrasenas([H|T], [H1|T2]):-
    primeroAndSegundo(H, H1),
    sacarNombresAndContrasenas(T, T2).

%---------------------------------------------------%
% Código Principal Plataforma que emula Google Docs %
%---------------------------------------------------%
% Dominios:
%   PD1: List (Plataforma ParadigmaDocs).
%   Username: String.
%   Password: String.
%   
%   
% Predicados:
%   paradigmaDocsRegister(PD1, Fecha, Username, Password, PD2).             aridad = 5
%   paradigmaDocsLogin(PD1, Username, Password, PD2).                       aridad = 4
%
% Metas:
%   Principales: paradigmaDocsRegister, paradigmaDocsLogin
%   Secundarias: selectNombreP, selectFechaP

%-----------------------%
% paradigmaDocsRegister %
%-----------------------%
/*
Predicado que permite registrar un usuario en la plataforma de paradigmadocs.
Este predicado no permite Usuarios con el mismo Username, por lo que verifica que no existan usuarios repetidos.
Además, cada usuario deberá registrarse con una contraseña y una fecha de creación de la cuenta.
*/
paradigmaDocsRegister(PD1, Fecha, Username, Password, PD2):-
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    usuario(Username, Password, Fecha, User1),
    sacarNombres(ListaReg, Nombres),
    \+miembro(Username, Nombres),
    append(ListaReg, [User1], ListaRegistroNueva),
    PD2 = [NombreP, FechaP, ListaRegistroNueva, UserActivo, ListaDocs].


%--------------------%
% paradigmaDocsLogin %
%--------------------%
/*
Predicado que autentica un usuario con su bombre de usuario y contraseña. Esto lo hace al verificar
que ese usuario que se está iniciando sesión, se encuentre anteriormente registrado en la plataforma.
Si se cumple esa condición se almacena en la pataforma como usuario activo y, caso contrario, se devuelve false.
*/
paradigmaDocsLogin(PD1, Username, Password, PD2):-
    string(Username),
    string(Password),
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectDocumentosP(PD1, ListaDocs),
    sacarNombresAndContrasenas(ListaReg, ListaNyC),
    miembro([Username, Password], ListaNyC),
    PD2 = [NombreP, FechaP, ListaReg, [Username], ListaDocs].


%---------------------%
% paradigmaDocsCreate %
%---------------------%
/*
Predicado que 
*/
paradigmaDocsCreate(PD1, Fecha, Nombre, Contenido, PD2):-
    selectUserActivoP(PD1, UserActivo),
    \+UserActivo==[],
    selectNombreUser(UserActivo, NombreUserActivo),
    documento(Nombre, Fecha, Contenido, DocumentoCreado),
    selectDocumentosP(PD1, ListaDocs),
    length(ListaDocs, NumId),
    setIdDoc(DocumentoCreado, NumId, DocumentoActualizado).



























%------------------------------%
% Ejemplos para cada predicado %
%------------------------------%
%
% paradigmaDocsRegister
%   Se registran 3 usuarios distintos
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4).
%   Se registran 3 usuarios, pero 2 se repiten nombre
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "alopez", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4).
%   Se registra 1 solo usuario
%       fecha(15, 12, 2021, F1), paradigmaDocs("google docs", F1, PD1), paradigmaDocsRegister(PD1, F1, "Griyitoxc", "paradigmaCLAVE", PD2).
%   UNIFICACIÓN del ultimo ejemplo
%       fecha(15, 12, 2021, [15, 12, 2021]), paradigmaDocs("google docs", [15, 12, 2021], ["google docs", [15, 12, 2021], [], [], []]), paradigmaDocsRegister(["google docs", [15, 12, 2021], [], [], []], [15, 12, 2021], "Griyitoxc", "paradigmaCLAVE", ["google docs", [15, 12, 2021], [["Griyitoxc", "paradigmaCLAVE", [15, 12, 2021]]], [], []]).
%
% paradigmaDocsLogin
%   Se Logea el usuario "vflores" con la contraseña "hola123"
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5).
%   Se Logea incorrectamente "vflores" con una contraseña errada "hola12"
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola12", PD5).
%   Se Logea incorrectamente un usuario que no existe
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "Griyitoxc", "PASSDEGRIyito", PD5).
%
% paradigmaDocsCreate
%
%
%
%
%
%
%
% paradigmaDocsShare
%
%
%
%
%
%
%
% paradigmaDocsAdd
%
%
%
%
%
%
%
% paradigmaDocsRestoreVersion
%
%
%
%
%
%
%
% paradigmaDocsToString
%
%
%
%
%
%
%