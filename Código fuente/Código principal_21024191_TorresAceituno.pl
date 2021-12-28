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
%    F1: una fecha
%
% Predicados
%    fecha(D, M, A, FOut). Aridad = 4                                                  
%    seleccionarDiaF([D, _, _], D). Aridad = 2
%    seleccionarMesF([_, M, _], M). Aridad = 2
%    seleccionarAnnoF([_, _, A], A). Aridad = 2
%    modificarDiaF(F1, D, F2). Aridad = 3
%    modificarMesF(F1, M, F2). Aridad = 3
%    modificarAnnoF(F1, A, F2). Aridad = 3
%    mesToString(M, MesEnString). Aridad = 2
%
% Clausulas
% Hechos
mes(1,31).
mes(2,28).
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
mesString(1, "Enero").
mesString(2, "Febrero").
mesString(3, "Marzo").
mesString(4, "Abril").
mesString(5, "Mayo").
mesString(6, "Junio").
mesString(7, "Julio").
mesString(8, "Agosto").
mesString(9, "Septiembre").
mesString(10, "Octubre").
mesString(11, "Noviembre").
mesString(12, "Diciembre").

% Reglas
% Constructor
fecha(D, M, A, Fecha):-
    integer(D), integer(M), integer(A), mes(M, Dias), D>0, D=<Dias, Fecha = [D, M, A].

% Selectores
seleccionarDiaF([D, _, _], D).
seleccionarMesF([_, M, _], M).
seleccionarAnnoF([_, _, A], A).

% Modificadores
modificarDiaF(F1, D, F2):-
    seleccionarMesF(F1, M1), seleccionarAnnoF(F1, A1), fecha(D, M1, A1, F2).
modificarMesF(F1, M, F2):-
    seleccionarDiaF(F1, D1), seleccionarAnnoF(F1, A1), fecha(D1, M, A1, F2).
modificarAnnoF(F1, A, F2):-
    seleccionarDiaF(F1, D1), seleccionarMesF(F1, M1), fecha(D1, M1, A, F2).

% Otros predicados del TDA
mesToString(M, MesEnString):- mesString(M, MesEnString).


%-------------------%
% TDA ParadigmaDocs %
%-------------------%
%
% Dominios
%    Name: String (Nombre de la plataforma)
%    Date: Fecha (Fecha de creación)
%    PD: plataforma paradigmadocs
%    ListaUser: List
%
% Predicados
%    paradigmaDocs(Name, Date, POut). Aridad = 3
%    selectNombreP(PD, NombreP). Aridad = 2
%    selectFechaP(PD, FechaP).
%    selectListaRegP(PD, ListaReg).
%    selectUserActivoP(PD, UserActivo).
%    selectDocumentosP(PD, ListaDocs).
%    selectNombreUser(ListaUser, User).
%
% Clausulas
% Constructor/Pertinencia/Modificadores
paradigmaDocs(Name, Date, ParadigmaDocs):-
    string(Name), ParadigmaDocs = [Name, Date, [], [], []].

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
%    User: String
%
% Predicados
%    usuario(Username, Password, Date, UOut). Aridad = 4
%    seleccionarUserPermiso(User, [Acc|AccT], Acc). Aridad = 3
%
% Clausulas
% Constructor/Pertinencia/Selectores/Modificadores
usuario(Username, Password, Date, User):-
    string(Username), string(Password), User = [Username, Password, Date].

% Selector
seleccionarUserPermiso(_, [], []):-!.
seleccionarUserPermiso(User, [Acc|AccT], Acc):-
    miembro(User, Acc),
    seleccionarUserPermiso(User, AccT, _), !.
seleccionarUserPermiso(User, [_|AccT], UsuarioConPermisos):-
    seleccionarUserPermiso(User, AccT, UsuarioConPermisos), !.


%---------------%
% TDA Documento %
%---------------%
%
% Dominios
%    Nombre: String (Nombre del documento)
%    Fecha: Fecha (Fecha de creación)
%    Doc: Documento
%    Id: Integer
%    ListaAccesos: Lista
%    ListaVersiones: Lista
%    NombreD: String
%    AutorD: String
%    FechaD: Fecha
%    Contenido: String
%
% Predicados
%    documento(Nombre, Fecha, Contenido, DOut). Aridad = 4
%    selectNombreD(Doc, NombreD). Aridad = 2
%    selectAutorD(Doc, AutorD). Aridad = 2
%    selectFechaD(Doc, FechaD). Aridad = 2
%    selectIdD(Doc, IdD). Aridad = 2
%    selectVersiones(Doc, ListaVersiones). Aridad = 2
%    selectAccesos(Doc, ListaAccesos). Aridad = 2 
%    setIdDoc(Doc, Id, Doc2). Aridad = 3
%    setAccesos(Doc, ListaAccesos, Doc2). Aridad = 3
%    setVersion(Doc, ListaVersiones, Doc2). Aridad = 3 
%
% Clausulas
% Constructor/Pertinencia
documento(Nombre, Autor, Fecha, Contenido, Doc):-
    string(Nombre), string(Autor), string(Contenido),
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
    selectNombreD(Doc1, NombreDoc), selectAutorD(Doc1, NombreAutorDoc), selectFechaD(Doc1, FechaDoc), selectIdD(Doc1, IdDoc), selectVersiones(Doc1, VerDoc),
    Doc2 = [NombreDoc, NombreAutorDoc, FechaDoc, IdDoc, VerDoc, ListaAccesos].

setVersion(Doc1, ListaVersiones, Doc2):-
    selectNombreD(Doc1, NombreDoc), selectAutorD(Doc1, NombreAutorDoc), selectFechaD(Doc1, FechaDoc), selectIdD(Doc1, IdDoc), selectAccesos(Doc1, AccDoc),
    Doc2 = [NombreDoc, NombreAutorDoc, FechaDoc, IdDoc, ListaVersiones, AccDoc].


%-------------%
% TDA Accesos %
%-------------%
%
% Dominios
%    Permisos: Lista
%    Usuarios: Permisos
%    ListaNombre: Lista
%    ListaAccesos: Lista
%    Name: String
%    User: String
%    Acceso: String
%
% Predicados
%    crearAccesos(Permisos, Usuarios, ListaAccesos). Aridad = 3
%    arregloAccesos(Lista, Lista). Aridad = 2
%    selectNombreAcceso(ListaNombre, Name). Aridad = 2
%    verificarPermisos(ListaAccesos). Aridad = 1
%    tienePermisoS(User). Aridad = 1
%    tienePermisoW(User). Aridad = 1
%    tienePermisoR(User). Aridad = 1
%    tienePermisoW(User). Aridad = 1
%    actualizarAccesos(ListaAccesos, Acceso, ListaAcceso). Aridad = 3
%    recorreAccesos(ListaAccesos, Lista, ListaAccesos). Aridad = 3
%
% Clausulas
% Predicado que aporta al constructor para cambiar formato de manera recursiva
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

% Selectores
selectNombreAcceso([Name|_], Name).

% Otros predicados de TDA Acceso
verificarPermisos([]):-!.
verificarPermisos([H|T]):-
    (H == "W"; H == "C"; H == "S"; H == "R"), !,
    verificarPermisos(T).

tienePermisoS(User):-
    miembro("S", User).

tienePermisoW(User):-
    miembro("W", User).

tienePermisoR(User):-
    miembro("R", User).

tienePermisoC(User):-
    miembro("C", User).

actualizarAccesos([], Acceso, [Acceso]).
actualizarAccesos([PrimerAcceso|SiguientesAccesos], Acceso, [Acceso|SiguientesAccesos]):-
    selectNombreAcceso(PrimerAcceso, Nombre),
    selectNombreAcceso(Acceso, Nombre), !.
actualizarAccesos([PrimerAcceso|SiguientesAccesos], Acceso, [PrimerAcceso|NuevosAccesos]):-
    actualizarAccesos(SiguientesAccesos, Acceso, NuevosAccesos).

recorreAccesos(ListaAccesos, [], ListaAccesos):- !.
recorreAccesos(ListaAccesos, [H|T], ListaRadom):-
    actualizarAccesos(ListaAccesos, H, ListaSalida),
    recorreAccesos(ListaSalida, T, ListaRadom).


%-------------------------------------------------------------------------------%
% Otros predicados (que ayudan al funcionamiento de los predicados principales) %
%-------------------------------------------------------------------------------%
miembro(X, [X|_]):-!.
    miembro(X, [_|T]):-miembro(X, T).

primero([H|_], H).
segundo([_, H|_], H).
tercero([_, _, H|_], H).
cuarto([_, _, _, H|_], H).
quinto([_, _, _, _, H|_], H).
sexto([_, _, _, _, _, H|_], H).

getNombresRegistrados([], []):-!.
getNombresRegistrados([H|T], [H1|T2]):-
    primero(H, H1),
    getNombresRegistrados(T, T2).

primeroAndSegundo([Name, Pass|_], [Name, Pass]).

sacarNombresAndContrasenas([], []):-!.
sacarNombresAndContrasenas([H|T], [H1|T2]):-
    primeroAndSegundo(H, H1),
    sacarNombresAndContrasenas(T, T2).

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

crearNuevaVersion(Contenido, Fecha, IdVersion, NuevaVersion):-
    NuevaVersion = [Contenido, Fecha, IdVersion].

agregarInicio(H, Lista, [H|Lista]).

prepararFecha(Fecha, FechaString):-
    seleccionarDiaF(Fecha, D),
    seleccionarMesF(Fecha, M),
    seleccionarAnnoF(Fecha, A),
    string_concat(D, " de ", F1),
    mesToString(M, M1),
    string_concat(F1, M1, F2),
    string_concat(F2, " de ", F3),
    string_concat(F3, A, FechaString).

prepararUsers([], []):-!.
prepararUsers([User|SigUsers], [UserString|SigUserString]):-
    primero(User, NombreU),
    segundo(User, ContrasennaU),
    tercero(User, FechaU),
    prepararFecha(FechaU, FechaString),
    string_concat("Nombre: ", NombreU, S1),
    string_concat(S1, " Contrasena: ", S2),
    string_concat(S2, ContrasennaU, S3),
    string_concat(S3, " Fecha de creacion: ", S4),
    string_concat(S4, FechaString, UserString),
    prepararUsers(SigUsers, SigUserString).

prepararAcceso([NombreUser|Accesos], AccesosString):-
    atomics_to_string(Accesos, ", ", AccesosEnString),
    ListaString = ["\n      Usuario: ", NombreUser, "\n      Permisos: ", AccesosEnString],
    atomics_to_string(ListaString, AccesosString).
    
prepararAccesos(ListaAccesos, ListaAccesosString):-
    maplist(prepararAcceso(), ListaAccesos, ListaAccesosString).

prepararVersion(Version, VersionString):-
    primero(Version, ContenidoV),
    segundo(Version, FechaV),
    tercero(Version, IdV),
    prepararFecha(FechaV, FechaEnString),
    ListaString = ["\n      Version: ", IdV, "\n      Contenido: ", ContenidoV, "\n      Fecha de modificacion: ", FechaEnString],
    atomics_to_string(ListaString, VersionString).

prepararVersiones(Versiones, VersionesEnString):-
    maplist(prepararVersion(), Versiones, VersionesEnString).

prepararDoc(Doc, DocString):-
    primero(Doc, TituloD),
    segundo(Doc, AutorD),
    tercero(Doc, FechaD),
    cuarto(Doc, IdD),
    quinto(Doc, VersionesD),
    sexto(Doc, AccesosD),
    % Preparación 
    prepararFecha(FechaD, FechaEnString),
    prepararAccesos(AccesosD, AccesosString),
    atomics_to_string(AccesosString, AccesosEnString),
    prepararVersiones(VersionesD, VersionesString),
    atomics_to_string(VersionesString, VersionesEnString),
    % Concatenación de string
    ListaString = ["\n   Documento Numero ", IdD, "\n   Nombre: ", TituloD, "\n   Autor: ", AutorD, "\n   Fecha de creacion: ", FechaEnString,
    "\n   Accesos:", AccesosEnString, "\n   Versiones (la ultima es la version activa):", VersionesEnString],
    atomics_to_string(ListaString, DocString).

prepararDocs(ListaDocs, ListaDocsString):-
    maplist(prepararDoc(), ListaDocs, ListaDocsString).

seleccionarDocPorAutor(_, [], []):-!.
seleccionarDocPorAutor(Autor, [Doc|SigDocs], [Doc|DocsSalidas]):-
    miembro(Autor, Doc),
    seleccionarDocPorAutor(Autor, SigDocs, DocsSalidas), !.
seleccionarDocPorAutor(Autor, [_|SigDocs], Salida):-
    seleccionarDocPorAutor(Autor, SigDocs, Salida), !.

seleccionarAccesosEnDocPorUser(_, [], []):-!.
seleccionarAccesosEnDocPorUser(User, [Doc|SigDocs], [Doc|DocsSalidas]):-
    selectAccesos(Doc, ListaAccesos),
    seleccionarUserPermiso(User, ListaAccesos, AccesoUser),
    miembro(User, AccesoUser),
    seleccionarAccesosEnDocPorUser(User, SigDocs, DocsSalidas), !.
seleccionarAccesosEnDocPorUser(User, [_|SigDocs], Salida):-
    seleccionarAccesosEnDocPorUser(User, SigDocs, Salida), !.

seleccionarUserRegistrado(_, [], []):-!.
seleccionarUserRegistrado(User, [Registrado|SigRegistrados], [Registrado|RegistradoSalidas]):-
    miembro(User, Registrado),
    seleccionarUserRegistrado(User, SigRegistrados, RegistradoSalidas), !.
seleccionarUserRegistrado(User, [_|SigRegistrados], RegTemp):-
    seleccionarUserRegistrado(User, SigRegistrados, RegTemp).
    
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
    getNombresRegistrados(ListaReg, Nombres),
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
Predicado que permite a un usuario logeado dar accesos en un documento específico por ID.
Este predicado tiene varias verificaciones (ejemplificadas). Entre ellas tenemos:
1. Si el usuario es dueño del documento seleccionado, este puede dar los permisos.
2. Si el usuario tiene permisos de compartir "S", puede dar permisos.
3. Los usuarios a los cuales se asignan permisos, deben estar registrados en la plataforma.
4. Los usuarios ingresados no pueden estar duplicados (se eliminan duplicados).
5. Los permisos ingresados no pueden estar duplicados (se eliminan duplicados).
6. El usuario debe estar correctamente logeado.
7. Los permisos ingresados deben ser los correctos ("C" = comentarios, "W" = escritura, "S" = compartir, "R" = lectura).
8. Los permisos son actualizables, esto quiere decir, que si un usuario ya tiene permisos otorgados anteriormente, al ingresar
nuevos permisos, estos se sobreescriben dentro de los permisos del usuario.
*/
paradigmaDocsShare(PD1, DocumentId, ListaPermisos, ListaUsernamesPermitidos, PD2):-
    % Verificaciones
    integer(DocumentId),
    \+ListaPermisos==[],
    \+ListaUsernamesPermitidos==[],
    verificarPermisos(ListaPermisos),
    eliminarDuplicados(ListaPermisos, ListaPermisos1),
    eliminarDuplicados(ListaUsernamesPermitidos, ListaUsernamesPermitidos1),
    % Getting info paradigmadocs
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    getNombresRegistrados(ListaReg, NombresRegistrados),
    % Verificar que los users nuevos estén registrados en la plataforma
    verificarRegistrados(ListaUsernamesPermitidos, NombresRegistrados),
    % Select user logeado y verificar que no sea vacío
    selectUserActivoP(PD1, UserActivo),
    \+UserActivo==[],
    % Select lista docs
    selectDocumentosP(PD1, ListaDocs),
    selectNombreUser(UserActivo, UserLogeado),
    % Crear accesos
    crearAccesos(ListaPermisos1, ListaUsernamesPermitidos1, ListaAccesosLista),
    % Selecciono el documento por ID
    myNth0(DocumentId, ListaDocs, DocById),
    % Saco sus accesos anteriores
    selectAccesos(DocById, AccesosDoc),
    % Selecciono autor
    selectAutorD(DocById, AutorDoc),
    % Selecciono si es que el user está logeado
    seleccionarUserPermiso(UserLogeado, AccesosDoc, UserDesdeAcceso),
    % Verificación si el usuario logeado es Autor del doc o tiene permisos de compartir "S"
    (tienePermisoS(UserDesdeAcceso), !; UserLogeado=AutorDoc, !),
    % Aplicar actualización recorriendo los accesos ListaAccesosLista
    recorreAccesos(AccesosDoc, ListaAccesosLista, ListaAccesosActualizados),
    setAccesos(DocById, ListaAccesosActualizados, DocConAccesos),
    reemplazar(ListaDocs, DocumentId, DocConAccesos, ListaDocsNueva),
    PD2 = [NombreP, FechaP, ListaReg, [], ListaDocsNueva].


%------------------%
% paradigmaDocsAdd %
%------------------%
/*
Predicado que permite a un usuario logeado añadir texto al final de una versión activa de un documento buscándolo por su ID.
Para que el usuario pueda hacer esta acción, debe tener permisos de escritura "W" o ser el propietario del documento.
*/
paradigmaDocsAdd(PD1, DocumentId, Fecha, ContenidoTexto, PD2):-
    % Verificaciones previas
    string(ContenidoTexto),
    integer(DocumentId),
    % Getting info paradigmadocs
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    % Verificar User logeado
    \+UserActivo==[],
    % Seleccionar el documento por ID y sacarle información
    myNth0(DocumentId, ListaDocs, DocById),
    % Seleccionarel nombre del user logeado
    selectNombreUser(UserActivo, UserLogeado),
    % Getting info del doc seleccionado
    selectAutorD(DocById, AutorDoc),
    selectAccesos(DocById, AccesosDoc),
    seleccionarUserPermiso(UserLogeado, AccesosDoc, UserDesdeAcceso),
    % Verificación si el usuario logeado es Autor del doc o tiene permisos de escritura """
    (tienePermisoW(UserDesdeAcceso), !; UserLogeado=AutorDoc, !),
    selectVersiones(DocById, ListaVersiones),
    % Seleccionar versión activa (primer índice)
    myNth0(0, ListaVersiones, VersionActivaDoc),
    % Sacar contenido para procesarlo con el contenido nuevo
    primero(VersionActivaDoc, ContenidoAntiguo),
    string_concat(ContenidoAntiguo, ContenidoTexto, ContenidoFinal),
    % Peparar versión nueva
    length(ListaVersiones, IdVersion),
    crearNuevaVersion(ContenidoFinal, Fecha, IdVersion, NuevaVersion),
    agregarInicio(NuevaVersion, ListaVersiones, NuevaListaVersiones),
    setVersion(DocById, NuevaListaVersiones, DocFinal),
    reemplazar(ListaDocs, DocumentId, DocFinal, ListaDocsNueva),
    PD2 = [NombreP, FechaP, ListaReg, [], ListaDocsNueva].


%------------------%
% paradigmaDocsAdd %
%------------------%
/*
Predicado que permite a un usuario logeado añadir texto al final de una versión activa de un documento buscándolo por su ID.
Para que el usuario pueda hacer esta acción, debe tener permisos de escritura "W" o ser el propietario del documento.
*/
paradigmaDocsRestoreVersion(PD1, DocumentId, IdVersion, PD2):-
    % Verificaciones previas
    integer(IdVersion),
    integer(DocumentId),
    % Getting info paradigmadocs
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectDocumentosP(PD1, ListaDocs),
    % Verificar User logeado
    \+UserActivo==[],
    % Seleccionar el documento por ID y sacarle información
    myNth0(DocumentId, ListaDocs, DocById),
    % Seleccionarel nombre del user logeado
    selectNombreUser(UserActivo, UserLogeado),
    % Verificar que el user logeado sea propietario
    selectAutorD(DocById, AutorDoc),
    UserLogeado==AutorDoc,
    selectVersiones(DocById, ListaVersiones),
    reverse(ListaVersiones, ListaVersionesReverse),
    % Seleccionar Id e info de la versión
    myNth0(IdVersion, ListaVersionesReverse, VersionPorRestaurar),
    primero(VersionPorRestaurar, ContenidoVersionPorRestaurar),
    segundo(VersionPorRestaurar, FechaVersionPorRestaurar),
    % Crear nueva versión
    length(ListaVersiones, IdVersionNueva),
    crearNuevaVersion(ContenidoVersionPorRestaurar, FechaVersionPorRestaurar, IdVersionNueva, NuevaVersion),
    agregarInicio(NuevaVersion, ListaVersiones, NuevaListaVersiones),
    setVersion(DocById, NuevaListaVersiones, DocFinal),
    reemplazar(ListaDocs, DocumentId, DocFinal, ListaDocsNueva),
    PD2 = [NombreP, FechaP, ListaReg, [], ListaDocsNueva].


%-----------------------%
% paradigmaDocsTostring %
%-----------------------%
/*
Predicado que genera una String de la plataforma paradigmaDocs.
Cuando se le pasa un paradigmaDocs que no tiene un usuario activo, se genera un string con todo el contenido almacenado en la plataforma y
caso contrario, se genera una string con toda la información relativa al usuario logeado, tal como los documentos los cuales es dueño,
los que tiene acceso y también, la información de su cuenta que esté almacenada en la plataforma.
El propósito de este predicado es generar una cadena de texto que pueda ser mostrada por consola a través de predicados como
display o write posteriormente.
*/
paradigmaDocsToString([NombreP, FechaP, ListaReg, [], ListaDocs], StringParadigmaDocs):-
    % CASO 1 NO HAY USER LOGEADO
    % Preparación primera parte (Info y users registrados) de paradigmaDocs to String
    prepararFecha(FechaP, FechaEnString),
    prepararUsers(ListaReg, RegistradosEnLista),
    atomics_to_string(RegistradosEnLista, "\n", RegistradosEnString),
    string_concat("#---------------#\n# PARADIGMADOCS #\n#---------------#\n\nNombre de la plataforma: ", NombreP, S1),
    string_concat(S1, "\nFecha de creacion: ", S2),
    string_concat(S2, FechaEnString, S3),
    string_concat(S3, "\nLos usuarios registrados son: \n", S4),
    string_concat(S4, RegistradosEnString, S5),
    % Preparación segunda parte (Docs) de paradigmaDocs to string
    prepararDocs(ListaDocs, ListaDocsString),
    atomics_to_string(ListaDocsString, ListaDocsEnString),
    string_concat(S5, "\nLos documentos son: ", S6),
    string_concat(S6, ListaDocsEnString, StringParadigmaDocs), !.
paradigmaDocsToString(PD1, StringParadigmaDocs):-
    % CASO 2 SI HAY USUARIO LOGEADO
    selectNombreP(PD1, NombreP),
    selectFechaP(PD1, FechaP),
    selectListaRegP(PD1, ListaReg),
    selectUserActivoP(PD1, UserActivo),
    selectNombreUser(UserActivo, UserLogeado),
    selectDocumentosP(PD1, ListaDocs),
    seleccionarUserRegistrado(UserLogeado, ListaReg, ListaInfoUser),
    primero(ListaInfoUser, InfoUser),
    segundo(InfoUser, Contrasenna),
    tercero(InfoUser, FechaU),
    prepararFecha(FechaU, FechaUserEnString),
    prepararFecha(FechaP, FechaEnString),
    string_concat("#---------------#\n# PARADIGMADOCS #\n#---------------#\n\nNombre de la plataforma: ", NombreP, S1),
    string_concat(S1, "\nFecha de creacion: ", S2),
    string_concat(S2, FechaEnString, S3),
    string_concat(S3, "\nUsuario activo: ", S4),
    string_concat(S4, UserLogeado, S5),
    string_concat(S5, "\nContrasena: ", S6),
    string_concat(S6, Contrasenna, S7),
    string_concat(S7, "\nFecha de creacion de la cuenta: ", S8),
    string_concat(S8, FechaUserEnString, S9),
    seleccionarDocPorAutor(UserLogeado, ListaDocs, ListaDocsUser),
    prepararDocs(ListaDocsUser, ListaDocsString),
    atomics_to_string(ListaDocsString, ListaDocsEnString),
    string_concat(S9, "\nLos documentos en los que el autor tiene autoria son:", S10),
    string_concat(S10, ListaDocsEnString, S11),
    seleccionarAccesosEnDocPorUser(UserLogeado, ListaDocs, DocsConAccesos),
    prepararDocs(DocsConAccesos, ListaDocsConAccesos),
    atomics_to_string(ListaDocsConAccesos, DocsConAccesosString),
    string_concat(S11, "\nLos documentos que el usuario tiene acceso son:", S12),
    string_concat(S12, DocsConAccesosString, StringParadigmaDocs), !.


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
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "Grillitoxc"], PD10).
%   Se comparten accesos incorrectos ["A", "R", "C"] a dos usuarios correctamente registrados ["vflores", "alopez"]
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["A", "R", "C"], ["vflores", "alopez"], PD10).
%   Se actualizan los permisos de "vflores" del ejemplo 1
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "vflores", "hola123", PD11), paradigmaDocsShare(PD11, 0, ["S","W"], ["vflores"], PD12).
%   Se logea un usuario que no es ni dueño ni tiene permisos ("crios")   
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "crios", "qwert", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10).
%   Se le da el permiso "S" share a "alopez". El dueño "vflores" es quién le da el permiso
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C", "S"], ["vflores", "alopez"], PD10).
%   Se logea "alopez" quien no es dueño del documento 0, pero tiene el permiso de compartir, por ende, puede actualizar o agregar permisos
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C", "S"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsShare(PD11, 0, ["R", "W"], ["vflores"], PD12).
%   Se logea "crios" y comparte a 2 personas más pero en el documento 1 que es de su autoría
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "crios", "qwert", PD13), paradigmaDocsShare(PD13, 1, ["W", "S", "R"], ["alopez", "crios"], PD14).  
%
% paradigmaDocsAdd
%   Se agrega "Más contenido 1" al documento 0. El usuario logeado es alguien que tiene permisos de escritura en este Doc
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12).
%   Se intenta agregar contenido a un documento (1) donde el usuario logeado no tiene permisos ni es dueño y falla
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 1, D1, " Más contenido 1", PD12).
%   Se agrega contenido a un documento por ID que no existe
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 2, D1, " Más contenido 1", PD12). 
%   
% paradigmaDocsRestoreVersion
%   Se restaura la versión 0 del documento 0
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "vflores", "hola123", PD13), paradigmaDocsRestoreVersion(PD13, 0, 0, PD14).
%   Se intenta restaurar una versión de un documento sin ser dueño de este
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "alopez", "asdfg", PD13), paradigmaDocsRestoreVersion(PD13, 0, 0, PD14).
%   Se restaura una versión inexistente
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "vflores", "hola123", PD13), paradigmaDocsRestoreVersion(PD13, 0, 9, PD14). 
%
% paradigmaDocsToString
%   Se muestra el contenido de toda la plataforma al no existir un usuario con sesión activa
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsToString(PD12, PDFINAL), write(PDFINAL).
%   Se muestra el contenido del usuario logeado "vFlores" de manera exitosa
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "vflores", "hola123", PD13), paradigmaDocsToString(PD13, PDFINAL), write(PDFINAL).
%   Se muestra el contenido del usuario logeado "crios" de manera exitosa
%       fecha(20, 12, 2015, D1), fecha(1, 12, 2021, D2), fecha(3, 12, 2021, D3), paradigmaDocs("google docs", D1, PD1), paradigmaDocsRegister(PD1, D2, "vflores", "hola123", PD2), paradigmaDocsRegister(PD2, D2, "crios", "qwert", PD3), paradigmaDocsRegister(PD3, D3, "alopez", "asdfg", PD4), paradigmaDocsLogin(PD4, "vflores", "hola123", PD5), paradigmaDocsCreate(PD5, D1, "Primer Título", "Contenido N°1", PD6), paradigmaDocsLogin(PD6, "crios", "qwert", PD7), paradigmaDocsCreate(PD7, D1, "Segundo Título", "Contenido N°2", PD8), paradigmaDocsLogin(PD8, "vflores", "hola123", PD9), paradigmaDocsShare(PD9, 0, ["W", "R", "C"], ["vflores", "alopez"], PD10), paradigmaDocsLogin(PD10, "alopez", "asdfg", PD11), paradigmaDocsAdd(PD11, 0, D1, " Más contenido 1", PD12), paradigmaDocsLogin(PD12, "crios", "qwert", PD13), paradigmaDocsShare(PD13, 1, ["W", "S", "R"], ["alopez", "crios"], PD14), paradigmaDocsLogin(PD14, "crios", "qwert", PD15), paradigmaDocsToString(PD15, PDFINAL), write(PDFINAL).
%