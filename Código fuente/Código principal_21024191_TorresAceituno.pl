% Nombre: Christopher Alejandro Torres Aceituno
% Rut: 21.024.191-4
% Proyecto de laboratorio N°2
% Plataforma que emula Google Docs

%-----------%
% TDA Fecha %
%-----------%
%
% Dominios
%    D: un número (representa el día).
%    M: un número (representa el mes).
%    A: un número (representa el año).
%
% Predicados
fecha(D, M, A, [D, M, A]):-
    integer(D), integer(M), integer(A), mes(M, Dias), D>0, D=<Dias.

%    
% Metas
%    Primarias:
%    Secundarias: 
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

% Constructor
% fecha(13,12,2021, Fecha).
% Fecha = [13, 12, 2021].


% Pertinencia
% fecha(_, _, _, [123, 12, 2021])

% Selectores
% fecha(Y, _, _, [13, 12, 2021]).
% fecha(_, M, _, [13, 12, 2021]).
% fecha(_, _, D, [13, 12, 2021]).

% Modificadores
% 