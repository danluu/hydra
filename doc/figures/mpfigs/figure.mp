%------------------------------------------------------------------------------
%
% Figures for Data Parallelism, by John O'Donnell
% 
%------------------------------------------------------------------------------

% This is a MetaPost program

input boxes
prologues:=0;

verbatimtex
\documentclass{article}
\begin{document}
\def\narrowoplus{\hbox{\hskip-.5pt$\oplus$\hskip-.5pt}}
etex;

u             = 2mm;  % basic unit of measure
cellhsep      = 1u;   % tree - horizontal space between cells
levelvsep     = 5u;   % tree - vertical separation between levels
textwidth     = 12.1cm;
midh          = textwidth/2;

portvoffset   = 3pt;
hpathangle    = 30;
ioporthoffset = 4u;
ioportvoffset = 1.5*u;
blackboxsep   = 4u;

iovportoffset = 2u;

%------------------------------------------------------------------------------
% Recursive definition of mscan

beginfig(1);
  
boxit.f(btex $f$ etex);
boxit.mscanf(btex \it mscan f etex);
mscanf.w=f.e+(blackboxsep,0);

circleit.inputa(btex $a$ etex);
inputa.c=f.w-(ioporthoffset,-ioportvoffset);
circleit.outputbpp(btex $b''$ etex);
outputbpp.c=f.w-(ioporthoffset,ioportvoffset);

circleit.inputx(btex $x$ etex);
inputx.s = f.n+(0,iovportoffset);
circleit.outputxp(btex $x'$ etex);
outputxp.n = f.s-(0,iovportoffset);
circleit.inputxs(btex \it xs etex);
inputxs.s = mscanf.n+(0,iovportoffset);
circleit.outputxsp(btex $\mathit{xs}'$ etex);
outputxsp.n = mscanf.s-(0,iovportoffset);

circleit.outputapp(btex $a''$ etex);
outputapp.c=mscanf.e+(ioporthoffset,ioportvoffset);
circleit.inputb(btex $b$ etex);
inputb.c=mscanf.e+(ioporthoffset,-ioportvoffset);

drawboxed(f,mscanf);
drawunboxed(inputa,outputbpp,inputx,outputxp,inputxs,outputxsp,
  outputapp,inputb);

drawarrow inputa.c{right} .. {right}f.w+(0,portvoffset)
  cutbefore bpath inputa;
drawarrow f.w+(0,-portvoffset){left} .. {left}outputbpp.c
  cutafter bpath outputbpp;
drawarrow inputx.s -- f.n;
drawarrow f.s -- outputxp.n;
drawarrow inputxs.s -- mscanf.n;
drawarrow mscanf.s -- outputxsp.n;
drawarrow f.e+(0,portvoffset){dir hpathangle}
  .. {dir (-hpathangle)}mscanf.w+(0,portvoffset);
label.top (btex $a'$ etex,
   1/2[f.e+(0,portvoffset),mscanf.w+(0,portvoffset)]);
drawarrow mscanf.w+(0,-portvoffset){dir (180+hpathangle)}
  .. {dir (180-hpathangle)}f.e+(0,-portvoffset);
label.bot (btex $b'$ etex,
   1/2[mscanf.w+(0,-portvoffset),f.e+(0,-portvoffset)]);
drawarrow mscanf.e+(0,portvoffset){right} .. {right}outputapp.c
  cutafter bpath outputapp;
drawarrow inputb.c{left} .. {left}mscanf.e+(0,-portvoffset)
  cutbefore bpath inputb;

endfig;

%------------------------------------------------------------------------------
% SIMD architecture: control, network, vector of P/M pairs

beginfig(2);

nprocs = 6;

boxjoin(b.w=a.e+(2u,0));
for i=1 upto nprocs:
  boxit.P[i](btex P etex);
endfor;
boxjoin();
for i=1 upto nprocs:
  boxit.M[i](btex M etex);
  P[i].sw = M[i].nw + (0,2u);
  P[i].se = M[i].ne + (0,2u);
  drawboxed(P[i],M[i]);
  draw P[i].s -- M[i].n;
  x[i] = xpart (P[i].n);
  y[i] = ypart (P[i].n) + 2u;
  draw P[i].n -- z[i];
endfor;

boxjoin();
boxit.netbox(btex Interconnection Network etex);
netbox.sw = P[1].nw + (0,2u);
netbox.se = P[6].ne + (0,2u);

boxit.ctlproc(btex Control etex);
ctlproc.s = netbox.n + (0,2u);


drawboxed (netbox,ctlproc);
draw ctlproc.s -- netbox.n;

endfig;

%------------------------------------------------------------------------------
% Tree architecture

beginfig(3);

circleit.node00(btex \small $n^0_0$ etex);
circleit.node10(btex \small $n^1_0$ etex);
circleit.node11(btex \small $n^1_1$ etex);
circleit.node20(btex \small $n^2_0$ etex);
circleit.node21(btex \small $n^2_1$ etex);
circleit.node22(btex \small $n^2_2$ etex);
circleit.node23(btex \small $n^2_3$ etex);

boxjoin(a.ne=a.sw+(3u,3u);b.sw=a.se+(cellhsep,0); b.nw=a.ne+(cellhsep,0));
boxit.cell0(btex \small $C_0$ etex);
boxit.cell1(btex \small $C_1$ etex);
boxit.cell2(btex \small $C_2$ etex);
boxit.cell3(btex \small $C_3$ etex);
boxit.cell4(btex \small $C_4$ etex);
boxit.cell5(btex \small $C_5$ etex);
boxit.cell6(btex \small $C_6$ etex);
boxit.cell7(btex \small $C_7$ etex);
boxjoin();

node00.c = 1/2[node10.c,node11.c] + (0,levelvsep);
node10.c = 1/2[node20.c,node21.c] + (0,levelvsep);
node11.c = 1/2[node22.c,node23.c] + (0,levelvsep);
node20.c = 1/2[cell0.c,cell1.c] + (0,levelvsep);
node21.c = 1/2[cell2.c,cell3.c] + (0,levelvsep);
node22.c = 1/2[cell4.c,cell5.c] + (0,levelvsep);
node23.c = 1/2[cell6.c,cell7.c] + (0,levelvsep);

drawboxed (cell0,cell1,cell2,cell3,cell4,cell5,cell6,cell7);
drawboxed (node20,node21,node22,node23,node10,node11,node00);

draw node20.c -- cell0.c cutbefore bpath node20 cutafter bpath cell0;
draw node20.c -- cell1.c cutbefore bpath node20 cutafter bpath cell1;
draw node21.c -- cell2.c cutbefore bpath node21 cutafter bpath cell2;
draw node21.c -- cell3.c cutbefore bpath node21 cutafter bpath cell3;
draw node22.c -- cell4.c cutbefore bpath node22 cutafter bpath cell4;
draw node22.c -- cell5.c cutbefore bpath node22 cutafter bpath cell5;
draw node23.c -- cell6.c cutbefore bpath node23 cutafter bpath cell6;
draw node23.c -- cell7.c cutbefore bpath node23 cutafter bpath cell7;

draw node10.c -- node20.c cutbefore bpath node10 cutafter bpath node20;
draw node10.c -- node21.c cutbefore bpath node10 cutafter bpath node21;
draw node11.c -- node22.c cutbefore bpath node11 cutafter bpath node22;
draw node11.c -- node23.c cutbefore bpath node11 cutafter bpath node23;

draw node00.c -- node10.c cutbefore bpath node00 cutafter bpath node10;
draw node00.c -- node11.c cutbefore bpath node00 cutafter bpath node11;

z0 = node00.n + (0,(1/2)*levelvsep);
draw z0 -- node00.c cutafter bpath node00;

endfig;

%------------------------------------------------------------------------------
% Tree implementation of mscanl (\small)

beginfig(4);

circleit.node00(btex \small $n^0_0$ etex);
circleit.node10(btex \small $n^1_0$ etex);
circleit.node11(btex \small $n^1_1$ etex);
circleit.node20(btex \small $n^2_0$ etex);
circleit.node21(btex \small $n^2_1$ etex);
circleit.node22(btex \small $n^2_2$ etex);
circleit.node23(btex \small $n^2_3$ etex);

boxjoin(a.ne=a.sw+(3u,3u);b.sw=a.se+(cellhsep,0); b.nw=a.ne+(cellhsep,0));
boxit.cell0(btex \small $x_0$ etex);
boxit.cell1(btex \small $x_1$ etex);
boxit.cell2(btex \small $x_2$ etex);
boxit.cell3(btex \small $x_3$ etex);
boxit.cell4(btex \small $x_4$ etex);
boxit.cell5(btex \small $x_5$ etex);
boxit.cell6(btex \small $x_6$ etex);
boxit.cell7(btex \small $x_7$ etex);
boxjoin();

node00.c = 1/2[node10.c,node11.c] + (0,levelvsep);
node10.c = 1/2[node20.c,node21.c] + (0,levelvsep);
node11.c = 1/2[node22.c,node23.c] + (0,levelvsep);
node20.c = 1/2[cell0.c,cell1.c] + (0,levelvsep);
node21.c = 1/2[cell2.c,cell3.c] + (0,levelvsep);
node22.c = 1/2[cell4.c,cell5.c] + (0,levelvsep);
node23.c = 1/2[cell6.c,cell7.c] + (0,levelvsep);


z100 = node00.n + (0,(1/2)*levelvsep);

drawboxed (cell0,cell1,cell2,cell3,cell4,cell5,cell6,cell7);
drawboxed (node20,node21,node22,node23,node10,node11,node00);

draw node20.c -- cell0.c cutbefore bpath node20 cutafter bpath cell0;
draw node20.c -- cell1.c cutbefore bpath node20 cutafter bpath cell1;
draw node21.c -- cell2.c cutbefore bpath node21 cutafter bpath cell2;
draw node21.c -- cell3.c cutbefore bpath node21 cutafter bpath cell3;
draw node22.c -- cell4.c cutbefore bpath node22 cutafter bpath cell4;
draw node22.c -- cell5.c cutbefore bpath node22 cutafter bpath cell5;
draw node23.c -- cell6.c cutbefore bpath node23 cutafter bpath cell6;
draw node23.c -- cell7.c cutbefore bpath node23 cutafter bpath cell7;

draw node10.c -- node20.c cutbefore bpath node10 cutafter bpath node20;
draw node10.c -- node21.c cutbefore bpath node10 cutafter bpath node21;
draw node11.c -- node22.c cutbefore bpath node11 cutafter bpath node22;
draw node11.c -- node23.c cutbefore bpath node11 cutafter bpath node23;

draw node00.c -- node10.c cutbefore bpath node00 cutafter bpath node10;
draw node00.c -- node11.c cutbefore bpath node00 cutafter bpath node11;

draw z100 -- node00.c cutafter bpath node00;


% Up from Node 00
z1000a = 1/2[node00.n,z100];
boxit.upnode00(btex \small 
  \vbox{\hbox{$\Uparrow\ ((x_0 \oplus x_1) \oplus (x_2 \oplus x_3)) \oplus$}
        \hbox{$\phantom{\Uparrow\ }((x_4 \oplus x_5) \oplus (x_6 \oplus x_7))$}
        \hbox{$\Downarrow\ a$}} etex);
upnode00.s = z100 + (0,2u);
fill bpath upnode00 withcolor .95white;
drawboxed (upnode00);



% Up from Node10
z10a = 1/2[node10.c,node00.c];
boxit.upnode10(btex \small \vbox{\hbox{$\Uparrow\ (x_0 \oplus x_1) \oplus
                                           (x_2 \oplus x_3)$}
                         \hbox{$\Downarrow\ a$}} etex);
xpart upnode10.nw = xpart z100 - midh;
ypart upnode10.nw = ypart upnode00.s - 1u;

fill bpath upnode10 withcolor .95white;
drawboxed (upnode10);
draw upnode10.e{right} .. {right}z10a dashed evenly;


% Up from Node11
z11a = 1/2[node11.c,node00.c];
boxit.upnode11(btex \small \vbox{\hbox{$\Uparrow\ (x_4 \oplus x_5) \oplus
                                           (x_6 \oplus x_7)$}
                         \hbox{$\Downarrow\ a \oplus
                                  ((x_0 \oplus x_1) \oplus
                                   (x_2 \oplus x_3))$}} etex);
%upnode11.ne = z100 + (midh,0);
xpart upnode11.ne = xpart z100 + midh;
ypart upnode11.ne = ypart upnode00.s - 1u;

fill bpath upnode11 withcolor .95white;
drawboxed (upnode11);
draw upnode11.w{left} .. {left}z11a dashed evenly;

% Up from Node 20
z20a = 1/2[node20.c,node10.c];
boxit.upnode20(btex \small \vbox{\hbox{$\Uparrow\ x_0 \oplus x_1$}
                         \hbox{$\Downarrow\ a$}} etex);
upnode20.nw = upnode10.sw - (0,1/2u);
fill bpath upnode20 withcolor .95white;
drawboxed (upnode20);
draw upnode20.e{right} .. {dir 315}z20a dashed evenly;

% Up from Node 21
z21a = 1/2[node21.c,node10.c];
boxit.upnode21(btex \small \vbox{\hbox{$\Uparrow\ x_2 \oplus x_3$}
                         \hbox{$\Downarrow\ a \oplus
                                   (x_0 \oplus x_1 )$}} etex);
upnode21.nw = upnode20.sw - (0,1/2u);
fill bpath upnode21 withcolor .95white;
drawboxed (upnode21);
draw upnode21.e{right} .. {right}z21a dashed evenly;



% Up from Node 22
z22a = 1/2[node22.c,node11.c];
boxit.upnode22(btex \small \vbox{\hbox{$\Uparrow\ x_4 \oplus x_5$}
                         \hbox{$\Downarrow\ a \oplus((x_0 \oplus x_1)$}
                         \hbox{$\phantom{\Downarrow\ } \oplus
                                  (x_2 \oplus x_3))$}} etex);
upnode22.ne = upnode11.se - (0,1/2u);
fill bpath upnode22 withcolor .95white;
drawboxed (upnode22);
draw upnode22.w{left} .. {left}z22a dashed evenly;


% Up from Node 23
z23a = 1/2[node23.c,node11.c];
boxit.upnode23(btex \small 
  \vbox{\hbox{$\Uparrow\ x_6 \oplus x_7$}
  \hbox{$\Downarrow\ (a \oplus$}
  \hbox{$\phantom{\Downarrow\ } ((x_0 \oplus x_1 )$}
  \hbox{$\phantom{\Downarrow\ } \oplus (x_2 \oplus x_3)))$}
  \hbox{$\phantom{\Downarrow\ } \oplus (x_4 \oplus x_5)$}
             } etex);
upnode23.ne = upnode22.se - (0,1/2u);
fill bpath upnode23 withcolor .95white;
drawboxed (upnode23);
draw upnode23.nw{dir 135} .. {dir 225}z23a dashed evenly;


% Up from Cell 0
z0a = 1/2[cell0.c,node20.c];
z0b = cell0.sw - (1u,1u);
boxit.upcell0(btex \small \vbox{\hbox{$\Uparrow\ x_0$}
                         \hbox{$\Downarrow\ a$}} etex);
ypart (upcell0.n) = ypart ( cell00.s) - 3u;
xpart (upcell0.w) = xpart (z100) - midh;

% Up from Cell 1
z1a = 1/2[cell1.c,node20.c];
z1b = cell1.sw - (1u,1u);
boxit.upcell1(btex \small \vbox{\hbox{$\Uparrow\ x_1$}
                         \hbox{$\Downarrow\ a \oplus x_0$}} etex);

cellnotesep = 1/2u;
upcell1.nw = upcell0.ne + (cellnotesep,0);

% Up from Cell 2
z2a = 1/2[cell2.c,node21.c];
z2b = cell2.sw - (1u,1u);
boxit.upcell2(btex \small \vbox{\hbox{$\Uparrow\ x_2$}
                         \hbox{$\Downarrow\ a \oplus$}
                         \hbox{$\phantom{\Downarrow\ }
                                   (x_0 \oplus x_1)$}} etex);
upcell2.nw = upcell1.ne + (cellnotesep,0);

% Up from Cell 3
z3a = 1/2[cell3.c,node21.c];
z3b = cell3.sw - (1u,1u);
boxit.upcell3(btex \small \vbox{\hbox{$\Uparrow\ x_3$}
                         \hbox{$\Downarrow\ (a \oplus$}
                         \hbox{$\phantom{\Downarrow\ }
                                 (x_0 \oplus x_1))$}
                         \hbox{$\phantom{\Downarrow\ }
                                 \oplus x_2$}} etex);
upcell3.ne = upcell2.se + (2u,-1u);

% Up from Cell 4
z4a = 1/2[cell4.c,node22.c];
z4b = cell4.sw - (1u,1u);
boxit.upcell4(btex \small 
   \vbox{\hbox{$\Uparrow\ x_4$}
         \hbox{$\Downarrow\ a \oplus ((x_0 \oplus x_1)$}
         \hbox{$\phantom{\Downarrow\ } \oplus (x_2 \oplus x_3))$}
          } etex);
upcell4.nw = upcell2.ne + (2u,0);

% Up from Cell 5
z5a = 1/2[cell5.c,node22.c];
z5b = cell5.sw - (1u,1u);
boxit.upcell5(btex \small 
   \vbox{\hbox{$\Uparrow\ x_5$}
         \hbox{$\Downarrow\ (a \oplus ((x_0 \oplus x_1)$}
         \hbox{$\phantom{\Downarrow\ } \oplus (x_2 \oplus x_3)))$}
         \hbox{$\phantom{\Downarrow\ } \oplus x_4$}
       } etex);
upcell5.nw = upcell3.ne + (2u,0);

% Up from Cell 6
z6a = 1/2[cell6.c,node23.c];
z6b = cell6.sw - (1u,1u);
boxit.upcell6(btex \small 
   \vbox{\hbox{$\Uparrow\ x_6$}
        \hbox{$\Downarrow\ (a \oplus ((x_0 \oplus x_1 )$}
        \hbox{$\phantom{\Downarrow\ } \oplus (x_2 \oplus x_3)))$}
        \hbox{$\phantom{\Downarrow\ } \oplus (x_4 \oplus x_5)$}
               } etex);

upcell6.nw = upcell4.ne + (4u,0);

% Up from Cell 7
z7a = 1/2[cell7.c,node23.c];
boxit.upcell7(btex \small 
  \vbox{\hbox{$\Uparrow\ x_7$}
        \hbox{$\Downarrow\ (a \oplus ((x_0 \oplus x_1 )$}
        \hbox{$\phantom{\Downarrow\ } \oplus (x_2 \oplus x_3)))$}
        \hbox{$\phantom{\Downarrow\ } \oplus (x_4 \oplus x_5)$}
               } etex);

%upcell7.nw = upcell5.ne + (2u,0);
xpart upcell7.e = xpart upnode11.e;
ypart upcell7.n = ypart upcell6.s - 1u;

%ypart (upcell7.n) = ypart ( cell7.s) - 3u;
%xpart (upcell7.e) = xpart (z100) + midh;


fill bpath upcell0 withcolor .95white;
drawboxed (upcell0);
draw upcell0.n{up} .. {right}z0a dashed evenly;
fill bpath upcell1 withcolor .95white;
drawboxed (upcell1);
draw upcell1.n{up} .. {right}z1a dashed evenly;
fill bpath upcell2 withcolor .95white;
drawboxed (upcell2);
draw upcell2.n{up} .. {right}z2a dashed evenly;
fill bpath upcell3 withcolor .95white;
drawboxed (upcell3);
draw upcell3.n{up} .. {right}z3a dashed evenly;
fill bpath upcell4 withcolor .95white;
drawboxed (upcell4);
draw upcell4.n{up} .. {right}z4a dashed evenly;
fill bpath upcell5 withcolor .95white;
drawboxed (upcell5);
draw upcell5.ne{up} .. {left}z5a dashed evenly;
fill bpath upcell6 withcolor .95white;
drawboxed (upcell6);
draw upcell6.nw{up} .. {left}z6a dashed evenly;
fill bpath upcell7 withcolor .95white;
drawboxed (upcell7);
draw upcell7.ne{up} .. {left}z7a dashed evenly;

draw upnode00.s{down} .. {right}z1000a dashed evenly;

endfig;

%------------------------------------------------------------------------------
% Recursive doubling network

beginfig(5);

boxjoin(b.nw=a.nw+(rdboxhsep,0));
boxit.A[1](btex \small $x_0$ etex);
boxit.A[2](btex \small $x_1$ etex);
boxit.A[3](btex \small $x_2$ etex);
boxit.A[4](btex \small $x_3$ etex);
boxit.A[5](btex \small $x_4$ etex);
boxit.A[6](btex \small $x_5$ etex);
boxit.A[7](btex \small $x_6$ etex);
boxit.A[8](btex \small $x_7$ etex);
for i=1 upto 8:
  A[i].e = A[i].w + (7.1u,0);
endfor;
boxjoin();

xpart A[8].e - xpart A[1].w = textwidth;

boxit.C[1](btex \small  $x_0$  etex);
boxit.C[2](btex \small  $x_0 \narrowoplus x_1$  etex);
boxit.C[3](btex \small 
  \vbox{\hbox{$x_0 \narrowoplus$}
        \hbox{$(x_1 \narrowoplus x_2)$}} etex);
boxit.C[4](btex \small 
  \vbox{\hbox{$(x_0 \narrowoplus x_1)$}
        \hbox{$\narrowoplus (x_2 \narrowoplus x_3)$}} etex);
boxit.C[5](btex \small 
  \vbox{\hbox{$(x_1 \narrowoplus x_2)$}
        \hbox{$\narrowoplus (x_3 \narrowoplus x_4)$}} etex);
boxit.C[6](btex \small 
  \vbox{\hbox{$(x_2 \narrowoplus x_3)$}
        \hbox{$\narrowoplus (x_4 \narrowoplus x_5)$}} etex);
boxit.C[7](btex \small 
  \vbox{\hbox{$(x_3 \narrowoplus x_4)$} \vfill
        \hbox{$\narrowoplus (x_5 \narrowoplus x_6)$}} etex);
boxit.C[8](btex \small 
  \vbox{\hbox{$(x_4 \narrowoplus x_5)$}
        \hbox{$\narrowoplus (x_6 \narrowoplus x_7)$}} etex);

boxit.D[1](btex \small  $x_0$  etex);
boxit.D[2](btex \small  $x_0 \narrowoplus x_1$ etex);
boxit.D[3](btex \small 
  \vbox{\hbox{$x_0 \narrowoplus$}
        \hbox{$(x_1 \narrowoplus x_2)$}} etex);
boxit.D[4](btex \small 
  \vbox{\hbox{$(x_0 \narrowoplus x_1)$}
        \hbox{$\narrowoplus (x_2 \narrowoplus x_3)$}} etex);
boxit.D[5](btex \small 
  \vbox{\hbox{$x_0 \narrowoplus$}
        \hbox{$((x_1 \narrowoplus x_2)$}
        \hbox{$\narrowoplus (x_3 \narrowoplus x_4))$}} etex);
boxit.D[6](btex \small 
  \vbox{\hbox{$(x_0 \narrowoplus x_1) \narrowoplus$}
        \hbox{$((x_2 \narrowoplus x_3)$}
        \hbox{$\narrowoplus (x_4 \narrowoplus x_5))$}} etex);
boxit.D[7](btex \small 
  \vbox{\hbox{$(x_0 \narrowoplus$}
        \hbox{$(x_1 \narrowoplus x_2)) \narrowoplus$}
        \hbox{$((x_3 \narrowoplus x_4)$}
        \hbox{$\narrowoplus (x_5 \narrowoplus x_6))$}} etex);
boxit.D[8](btex \small
  \vbox{\hbox{$((x_0 \narrowoplus x_1)$}
        \hbox{$\narrowoplus (x_2 \narrowoplus x_3))$}
        \hbox{$\narrowoplus((x_4 \narrowoplus x_5)$}
        \hbox{$\narrowoplus (x_6 \narrowoplus x_7))$}} etex);


for i = 2 upto 8:
%  ypart (B[i].n) = ypart (B[i-1].n);
%  ypart (B[i].s) = ypart (B[i-1].s);
  ypart (C[i].n) = ypart (C[i-1].n);
  ypart (C[i].s) = ypart (C[i-1].s);
  ypart (D[i].n) = ypart (D[i-1].n);
  ypart (D[i].s) = ypart (D[i-1].s);
endfor;

boxit.B[1](btex \small $x_0$ etex);
boxit.B[2](btex \small $x_0 \narrowoplus x_1$ etex);
boxit.B[3](btex \small $x_1 \narrowoplus x_2$ etex);
boxit.B[4](btex \small $x_2 \narrowoplus x_3$ etex);
boxit.B[5](btex \small $x_3 \narrowoplus x_4$ etex);
boxit.B[6](btex \small $x_4 \narrowoplus x_5$ etex);
boxit.B[7](btex \small $x_5 \narrowoplus x_6$ etex);
boxit.B[8](btex \small $x_6 \narrowoplus x_7$ etex);

for i=1 upto 8:
  B[i].nw = A[i].sw - (0,2u);
  B[i].ne = A[i].se - (0,2u);
  C[i].nw = B[i].sw - (0,5u);
  C[i].ne = B[i].se - (0,5u);
  D[i].nw = C[i].sw - (0,10u);
  D[i].ne = C[i].se - (0,10u);
endfor;

drawunboxed (A[8],B[8],C[8],D[8]);
for i=1 upto 8:
  fill bpath A[i] withcolor .90white;
  fill bpath B[i] withcolor .90white;
  fill bpath C[i] withcolor .90white;
  fill bpath D[i] withcolor .90white;
endfor;
drawunboxed (A[8],B[8],C[8],D[8]);
for i=1 upto 7:
  drawunboxed (A[i],B[i],C[i],D[i]);
endfor;

for i=1 upto 8:
  drawarrow A[i].s -- B[i].n;
  drawarrow B[i].s -- C[i].n;
  drawarrow C[i].s -- D[i].n;
endfor;

for i=1 upto 7:
  drawarrow 1/2[A[i].se,A[i].s] -- 1/2[B[i+1].nw,B[i+1].n];
endfor;

for i=1 upto 6:
  drawarrow 1/2[B[i].se,B[i].s] -- 1/2[C[i+2].nw,C[i+2].n];
endfor;

for i=1 upto 4:
  drawarrow 1/2[C[i].se,C[i].s] -- 1/2[D[i+4].nw,D[i+4].n];
endfor;

endfig;


%------------------------------------------------------------------------------
% foldl

beginfig(6);
  
boxjoin (b.w=a.e+(3u,0));
for i=1 upto 4:
  boxit.Ufoldl[i](btex $f$ etex);
  drawboxed(Ufoldl[i]);
endfor;
boxjoin();
boxit.foldlx[1](btex $x_0$ etex);
boxit.foldlx[2](btex $x_1$ etex);
boxit.foldlx[3](btex $x_2$ etex);
boxit.foldlx[4](btex $x_3$ etex);
for i=1 upto 4:
  foldlx[i].s = Ufoldl[i].n + (0,3u);
  drawunboxed(foldlx[i]);
  drawarrow foldlx[i].s -- Ufoldl[i].n;
endfor;

pair leftport,rightport;
leftport = Ufoldl[1].w-(3u,0);
rightport = Ufoldl[4].e+(3u,0);

drawarrow leftport -- Ufoldl[1].w;
label.lft (btex $a$ etex, leftport);
for i=1 upto 3:
  drawarrow Ufoldl[i].e -- Ufoldl[i+1].w;
endfor;
drawarrow Ufoldl[4].e -- rightport;
label.rt (btex $z$ etex, rightport);

endfig;

%------------------------------------------------------------------------------
% scanl (exclusive)

beginfig(7);
  
boxjoin (b.w=a.e+(3u,0));
for i=1 upto 4:
  boxit.Uscanl[i](btex $f$ etex);
  drawboxed(Uscanl[i]);
endfor;
boxjoin();
boxit.scanlx[1](btex $x_0$ etex);
boxit.scanlx[2](btex $x_1$ etex);
boxit.scanlx[3](btex $x_2$ etex);
boxit.scanlx[4](btex $x_3$ etex);
for i=1 upto 4:
  scanlx[i].s = Uscanl[i].n + (0,3u);
  drawunboxed(scanlx[i]);
  drawarrow scanlx[i].s -- Uscanl[i].n;
endfor;

pair leftport,rightport,scanpt[];
leftport = Uscanl[1].w-(3u,0);
rightport = Uscanl[4].e+(3u,0);

drawarrow leftport -- Uscanl[1].w;
scanpt[1] = 1/2[leftport,Uscanl[1].w];
scanpt[2] = 1/2[Uscanl[1].e,Uscanl[2].w];
scanpt[3] = 1/2[Uscanl[2].e,Uscanl[3].w];
scanpt[4] = 1/2[Uscanl[3].e,Uscanl[4].w];

for i=1 upto 4:
  drawarrow scanpt[i] -- scanpt[i]-(0,3.5u);
endfor;
label.bot (btex $y_0$ etex, scanpt[1]-(0,3.5u));
label.bot (btex $y_1$ etex, scanpt[2]-(0,3.5u));
label.bot (btex $y_2$ etex, scanpt[3]-(0,3.5u));
label.bot (btex $y_3$ etex, scanpt[4]-(0,3.5u));

label.lft (btex $a$ etex, leftport);
for i=1 upto 3:
  drawarrow Uscanl[i].e -- Uscanl[i+1].w;
endfor;
drawarrow Uscanl[4].e -- rightport;
label.rt (btex $z$ etex, rightport);

endfig;


%------------------------------------------------------------------------------
% scanl (inclusive)

beginfig(8);
  
boxjoin (b.w=a.e+(3u,0));
for i=1 upto 4:
  boxit.Uscanl[i](btex $f$ etex);
  drawboxed(Uscanl[i]);
endfor;
boxjoin();
boxit.scanlx[1](btex $x_0$ etex);
boxit.scanlx[2](btex $x_1$ etex);
boxit.scanlx[3](btex $x_2$ etex);
boxit.scanlx[4](btex $x_3$ etex);
for i=1 upto 4:
  scanlx[i].s = Uscanl[i].n + (0,3u);
  drawunboxed(scanlx[i]);
  drawarrow scanlx[i].s -- Uscanl[i].n;
endfor;

pair upleftport,uprightport,scanpt[],dnleftport,dnrightport;

upleftport = 1/3[Uscanl[1].nw,Uscanl[1].sw]-(3u,-1.5u);
dnleftport = 2/3[Uscanl[1].nw,Uscanl[1].sw]-(3u,1.5u);
uprightport = 1/3[Uscanl[4].ne,Uscanl[4].se]+(3u,1.5u);
dnrightport = 2/3[Uscanl[4].ne,Uscanl[4].se]+(3u,-1.5u);

scanpt[1] = Uscanl[1].s;
scanpt[2] = Uscanl[2].s;
scanpt[3] = Uscanl[3].s;
scanpt[4] = Uscanl[4].s;

for i=1 upto 4:
  drawarrow scanpt[i] -- scanpt[i]-(0,3u);
endfor;
label.bot (btex $y_0$ etex, scanpt[1]-(0,3u));
label.bot (btex $y_1$ etex, scanpt[2]-(0,3u));
label.bot (btex $y_2$ etex, scanpt[3]-(0,3u));
label.bot (btex $y_3$ etex, scanpt[4]-(0,3u));


for i=1 upto 3:
  drawarrow 1/3[Uscanl[i].ne,Uscanl[i].se] --
            1/3[Uscanl[i+1].nw,Uscanl[i+1].sw];
  drawarrow 2/3[Uscanl[i+1].nw,Uscanl[i+1].sw] --
            2/3[Uscanl[i].ne,Uscanl[i].se];
endfor;

drawarrow upleftport{right} .. {right}1/3[Uscanl[1].nw,Uscanl[1].sw];
label.lft (btex $a$ etex, upleftport);
drawarrow  2/3[Uscanl[1].nw,Uscanl[1].sw]{left} .. {left}dnleftport;
label.lft (btex $b'$ etex, dnleftport);

drawarrow 1/3[Uscanl[4].ne,Uscanl[4].se]{right} .. {right}uprightport;
label.rt (btex $a'$ etex, uprightport);
drawarrow dnrightport{left} .. {left}2/3[Uscanl[4].ne,Uscanl[4].se];
label.rt (btex $b$ etex, dnrightport);

endfig;


%------------------------------------------------------------------------------
% mscanl

beginfig(9);
  
boxjoin (b.w=a.e+(3u,0));
for i=1 upto 4:
  boxit.Vscanl[i](btex $f$ etex);
  drawboxed(Vscanl[i]);
endfor;
boxjoin();
boxit.scanlx[1](btex $x_0$ etex);
boxit.scanlx[2](btex $x_1$ etex);
boxit.scanlx[3](btex $x_2$ etex);
boxit.scanlx[4](btex $x_3$ etex);
for i=1 upto 4:
  scanlx[i].s = Vscanl[i].n + (0,3u);
  drawunboxed(scanlx[i]);
  drawarrow scanlx[i].s -- Vscanl[i].n;
endfor;

pair leftport,rightport,scanpt[];
leftport = Vscanl[1].w-(3u,0);
rightport = Vscanl[4].e+(3u,0);

drawarrow leftport -- Vscanl[1].w;
scanpt[1] = Vscanl[1].s;
scanpt[2] = Vscanl[2].s;
scanpt[3] = Vscanl[3].s;
scanpt[4] = Vscanl[4].s;

for i=1 upto 4:
  drawarrow scanpt[i] -- scanpt[i]-(0,3u);
endfor;
label.bot (btex $y_0$ etex, scanpt[1]-(0,3u));
label.bot (btex $y_1$ etex, scanpt[2]-(0,3u));
label.bot (btex $y_2$ etex, scanpt[3]-(0,3u));
label.bot (btex $y_3$ etex, scanpt[4]-(0,3u));

label.lft (btex $a$ etex, leftport);
for i=1 upto 3:
  drawarrow Vscanl[i].e -- Vscanl[i+1].w;
endfor;
drawarrow Vscanl[4].e -- rightport;
label.rt (btex $a'$ etex, rightport);

endfig;

%------------------------------------------------------------------------------
end
