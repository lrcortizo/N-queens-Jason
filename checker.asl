
/* Initial beliefs */

/* Initial goal */

!start.
/* Plans */

+!start : playAs(0) <-
	?size(N);
	.print("Sabemos el tamaño del tablero:", N);
	!initiateBB(N);
	.print("Iniciamos la partida.");
	!nextPlay.
	
+!start <- ?size(N); !initiateBB(N).

+!start : not size(N) <- !start.

//-----------------------------------------JUGADORES----------------------------------------------
+player(N): playAs(N) <-.wait(1000); .print("Me toca jugar"); !pedir; !nextPlay. 
+player(0): not playAs(_) <- !obstaculo.

//---------------------------------------INICIAR TABLERO--------------------------------------------------
+!initiateBB(N) : true <- 
	.print("Inicializando el modelo del tablero.");
	+maxObstaculos(N/2); //  N/4 bloques + N/4 agujeros = N/2
	+maxQueens(N/2);
	for (.range(I,0,N-1)){
		for (.range(J,0,N-1)){
			+free(I,J);
			}
		};
	.print("Finalizada la creacion del modelo del tablero.").

//-------------------------------------SOLICITAR BLOQUES O AGUJEROS-------------------------------------------
+!pedir: playAs(_) & maxObstaculos(N) & N > 0 & free(X,Y)<-
	?size(S);
	.random(T);
	if(T<0.5){
		.send(configurer,tell,block(S-X,S-Y));
	} else {
		.send(configurer,tell,hole(S-X,S-Y));
	}.
-!pedir.

//---------------------------------------PONER BLOQUES O AGUJEROS----------------------------------------------
+!obstaculo : blancas(Pb,Xb,Yb) & negras(Pn, Xn, Yn) & not updateReina<-
	+updateObstaculo;
	.print("Colocando obstaculo");
	if((not free(Xb,Yb)) & (not free(Xn, Yn))){
		?free(X,Y);
		block(X,Y);
	}
	if((free(Xb,Yb)) & (not free(Xn, Yn))){
		if(Pb==1){
			block(Xb,Yb);
		}else{
			hole(Xb,Yb);
		}
	}
	if((not free(Xb,Yb)) & (free(Xn, Yn))){
		if(Pn==1){
			block(Xn,Yn);
		}else{
			hole(Xn,Yn);
		}		
	}
	if((free(Xb,Yb)) & (free(Xn, Yn))){
		.random(T);
		if(T<0.5){
			if(Pn==1){
				block(Xn,Yn);
			}else{
				hole(Xn,Yn);
				}	
		} else{
			if(Pb==1){
				block(Xb,Yb);
			}else{
				hole(Xb,Yb);
				}
		}
	}.
-!obstaculo <- !obstaculo.
//--------------------------------------------PONER REINAS-------------------------------------------------------------
+!nextPlay : maxQueens(N) & N>0 & not updateObstaculo <-
	+updateReina;
	.findall(pos(A,B),free(A,B),Lista);
	.print("Posiciones libres ", Lista);
	?free(X,Y);
	.print("Colocando reina: ");
	queen(X,Y);
	-+maxQueens(N-1).
-!nextPlay : not free(X,Y) | (maxQueens(N) & N=0) <- .print("Fin de la partida").
-!nextPlay <- !nextPlay.

//-------------------------------------------RECIBIR SOLICITUD AGUJERO---------------------------------------------------
+hole(X,Y)[source(Ag)]: not Ag=percept <-
	if(white==Ag){
		+blancas(0,X,Y);
	}
	if(black==Ag){
		+negras(0,X,Y);
	}.
	
//----------------------------------------QUITAR LIBRE-------------------------
+hole(X,Y) <- +agujero(X,Y); -free(X,Y); -updateObstaculo.
	
//-------------------------------------RECIBIR SOLICITUD BLOQUE-----------------------------------------
+block(X,Y)[source(Ag)]: not Ag=percept <-
	if(white==Ag){
		+blancas(1,X,Y);
	}
	if(black==Ag){
		+negras(1,X,Y);
	}.

//-------------------------REINICIAR TABLERO-------------------------------
+block(X,Y): playAs(_) & size(N) <-
	+bloque(X,Y);
	for (.range(I,0,N-1)){
		for (.range(J,0,N-1)){
			if(not free(I,J)){
				+free(I,J);
			}
		}
	};
	.findall(b(A,B),bloque(A,B)|agujero(A,B),ListB);
	for(.member(b(A,B),ListB)){
		-free(A,B);
	}
	.findall(r(C,D),reina(C,D),ListR);
	for(.member(r(C,D),ListR)){
		!removeFree(C,D);
	}
	.wait(1000);
	-updateObstaculo.

//-----------------------------------------QUITAR LIBRES------------------------------------------------------------
+queen(X,Y) <- +reina(X,Y); !removeFree(X,Y). 

+!removeFree(X,Y) : size(N) <-
	-free(X,Y);
	!removeFreeDer(X+1,Y);
	!removeFreeIzq(X-1,Y);
	!removeFreeAbajo(X,Y+1);
	!removeFreeArriba(X,Y-1);
	!removeFreeDigMayDer(X+1,Y+1);
	!removeFreeDigMayIzq(X-1,Y-1);
	!removeFreeDigMenDer(X+1,Y-1);
	!removeFreeDigMenIzq(X-1,Y+1);
	-updateReina.
	
	//fila derecha
+!removeFreeDer(X,Y):  size(N) & not block(X,Y) & X<N <-
	-free(X,Y);
	!removeFreeDer(X+1,Y).
-!removeFreeDer(X,Y).
	//fila izquierda
+!removeFreeIzq(X,Y): size(N) & not block(X,Y) & X > -1 <-
	-free(X,Y);
	!removeFreeIzq(X-1,Y).
-!removeFreeIzq(X,Y).
	//columna abajo
+!removeFreeAbajo(X,Y): size(N) & not block(X,Y) & Y<N <-
	-free(X,Y);
	!removeFreeAbajo(X,Y+1).
-!removeFreeAbajo(X,Y).
	//columna arriba
+!removeFreeArriba(X,Y): size(N) & not block(X,Y) & Y> -1 <-
	-free(X,Y);
	!removeFreeArriba(X,Y-1).
-!removeFreeArriba(X,Y).
	//diagonal mayor derecha
+!removeFreeDigMayDer(X,Y): size(N) & not block(X,Y) & Y<N & X<N <-
	-free(X,Y);
	!removeFreeDigMayDer(X+1,Y+1).
-!removeFreeDigMayDer(X,Y).
	//diagonal mayor izquierda
+!removeFreeDigMayIzq(X,Y): size(N) & not block(X,Y) & Y > -1 & X > -1 <-
	-free(X,Y);
	!removeFreeDigMayIzq(X-1,Y-1).
-!removeFreeDigMayIzq(X,Y).
	//diagonal menor derecha
+!removeFreeDigMenDer(X,Y): size(N) & not block(X,Y) & X<N & Y> -1 <-
	-free(X,Y);
	!removeFreeDigMenDer(X+1,Y-1).
-!removeFreeDigMenDer(X,Y).
	//diagonal menor izquierda
+!removeFreeDigMenIzq(X,Y): size(N) & not block(X,Y) & X > -1 & Y<N <-
	-free(X,Y);
	!removeFreeDigMenIzq(X-1,Y+1).
-!removeFreeDigMenIzq(X,Y).
	



