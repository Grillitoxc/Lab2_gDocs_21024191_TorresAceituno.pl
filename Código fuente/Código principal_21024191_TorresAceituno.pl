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
documento(Nombre, Autor, Fecha, Contenido, Doc):-
    string(Nombre), string(Autor), esFecha(Fecha), string(Contenido),
    Doc = [Nombre, Autor, Fecha, "", [[Contenido, Fecha, 0]], []].

% Selectores
selectNombreD([NombreD, _, _, _, _, _], NombreD).
selectAutorD([_, AutorD, _, _, _, _], AutorD).
selectFechaD([_, _, FechaD, _, _, _], FechaD).
selectIdD([_, _, _, IdD, _, _], IdD).
selectVersiones([_, _, _, _, ListaVer, _], ListaVer).
selectAccesos([_, _, _, _, _, ListaAccesos], ListaAccesos).

% Modificadores
setIdDoc(Doc1, Id, Doc2):-
    selectNombreD(Doc1, NombreDoc), selectAutorD(Doc1, NombreAutorDoc), selectFechaD(Doc1, FechaDoc), selectVersiones(Doc1, VerDoc), selectAccesos(Doc1, AccDoc),
    Doc2 = [NombreDoc, NombreAutorDoc, FechaDoc, Id, VerDoc, AccDoc].

setAccesos(Doc1, ListaAccesos, Doc2):-
    selectNombreD(Doc1, NombreDoc), selectAutorD(Doc1, NombreAutorDoc), selectFechaD(Doc1, FechaDoc), selectIdD(Doc1, IdDoc), selectVersiones(Doc1, VerDoc), selectAccesos(Doc1, AccDoc),
    append(ListaAccesos, AccDoc, AccDoc1),
    Doc2 = [NombreDoc, NombreAutorDoc, FechaDoc, IdDoc, VerDoc, AccDoc1].



%-------------%
% TDA Accesos %
%-------------%
%
% Dominios
%    Nombre: String (Nombre del documento)
%    Fecha: Fecha (Fecha de creación)
%
% Predicados
%    documento(Nombre, Fecha, Contenido, DOut)                              aridad = 4
%
% Clausulas
% Función que aporta al constructor para cambiar formato de manera recursiva
arregloAccesos([], []):-!.
arregloAccesos([H|T], [[H]|T1]):-
    arregloAccesos(T, T1).

% Constructor
crearAccesos(Permisos, Usuarios, ListaAccesos):-
    verificarPermisos(Permisos),
    is_list(Permisos),
    is_list(Usuarios),
    arregloAccesos(Usuarios, UsuariosNew),
    maplist(append(Permisos), UsuariosNew, ListaAcc),
    maplist(reverse, ListaAcc, ListaAccesos).


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

verificarPermisos([]):-!.
verificarPermisos([H|T]):-
    (H == "W"; H == "C"; H == "S"; H == "R"), !,
    verificarPermisos(T).

verificarRegistrados([], _):-!.
verificarRegistrados([H|T], Lista):-
    miembro(H, Lista),
    verificarRegistrados(T, Lista).

eliminarDuplicados([], []):-!.
eliminarDuplicados([H | T], Lista):-
    member(H, T), !,
    eliminarDuplicados(T, Lista).
eliminarDuplicados([H | T], [H | Lista]):-
    eliminarDuplicados(T, Lista).

myNth0(0, [H|_], H):-!.
myNth0(Indice, [_|T], E):-
    Indice1 is Indice - 1,
    myNth0(Indice1, T, E).

reemplazar([_|T], 0, X, [X|T]):-!.
reemplazar([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    reemplazar(T, I1, X, R).
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
Predicado que crea un documento con la autoría del usuario logeado. Si el usuario no está logeado correctamente
el predicado falla. Una vez creado el documento nuevo con el contenido, fecha de creación y título, se inserta a la plataforma
con su respectivo ID verificador.
*/
paradigmaDocsCreate(PD1, Fecha, Nombre, Contenido, PD2):-
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    \+UserActivo==[],
    selectNombreUser(UserActivo, NombreUserActivo),
    documento(Nombre, NombreUserActivo, Fecha, Contenido, DocumentoCreado),
    selectDocumentosP(PD1, ListaDocs),
    length(ListaDocs, NumId),
    setIdDoc(DocumentoCreado, NumId, DocumentoActualizado),
    append(ListaDocs, [DocumentoActualizado], ListaDocsNueva),
    PD2 = [NombreP, FechaP, ListaReg, [], ListaDocsNueva].


%---------------------%
% paradigmaDocsShare %
%---------------------%
/*
Predicado que permite a un usuario logeado dar accesos en un documento específico por ID verificando que estos sean
"C" = comentarios, "W" = escritura, "S" = compartir, "R" = lectura.
*/
paradigmaDocsShare(PD1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, PD2):-
    \+ListaPermisos==[],
    \+ListaUsernamesPermitidos==[],
    verificarPermisos(ListaPermisos),
    eliminarDuplicados(ListaPermisos, ListaPermisos1),
    eliminarDuplicados(ListaUsernamesPermitidos, ListaUsernamesPermitidos1),
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    sacarNombres(ListaReg, NombresRegistrados),
    verificarRegistrados(ListaUsernamesPermitidos, NombresRegistrados),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    \+UserActivo==[],
    crearAccesos(ListaPermisos1, ListaUsernamesPermitidos1, ListaAccesosLista),
    myNth0(DocumentId, ListaDocs, DocById),
    setAccesos(DocById, ListaAccesosLista, DocConAccesos),
    reemplazar(ListaDocs, DocumentId, DocConAccesos, ListaDocsNueva),
    PD2 = [NombreP, FechaP, ListaReg, [], ListaDocsNueva].






















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
%   Se crea un documento con autor "vflores" (usuario logeado) y se guarda en la plataforma
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6).
%   No se crea un documento ya que el usuario no está logeado anteriormente
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6).
%   Se crean dos documentos correctamente con sus respectivos login
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8).
%
% paradigmaDocsShare
%   Se comparten tres accesos ["W", "R", "C"] a dos usuarios registrados ["vflores", "alopez"] en el documento 0
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10).
%   Se comparten tres accesos ["W", "R", "C"] a dos usuarios pero uno sin registrar ["vflores", "Griyitoxc"] y falla
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "Grillitoxc"], PD10)
%   Se comparten accesos incorrectos ["A", "R", "C"] a dos usuarios correctamente registrados ["vflores", "alopez"]
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["A", "R", "C"], ["vflores", "alopez"], PD10).
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