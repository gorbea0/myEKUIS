/**
* @file ekuis.c
* @version 1.0
* @Author Oscar Unzueta
* @date 26/10/2020
* @brief generacion archivo xml para exportar datos analiticos del lims al EKUIS
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define MAXanalitica 280
#define MAXregistro 90



char *unidades (char const *paramet){
	//obtiene las unidades recibiendo el parametro de analisis
	if (strcmp(paramet,"PH")==0){	return "-";	
	}
	  else if (strcmp(paramet,"CONDUC")==0){	return "µS/cm";	
	}
	  else if (strcmp(paramet,"TURB")==0){	return "UNF";	
	}
	
	  else if (strcmp(paramet,"BH22")==0){	return "UFC/ml";	
	}
	
    else if (strcmp(paramet,"COTO")==0 || strcmp(paramet,"ENTCO")==0  || strcmp(paramet,"ECOLI")==0   || strcmp(paramet,"CLPER")==0   ){
	return "UFC/100 ml";	
	}
     	else if (strcmp(paramet,"ALD")==0 || strcmp(paramet,"HPCL")==0 || strcmp(paramet,"HPCLE")==0  || strcmp(paramet,"DIEL")==0 ){	return "ng/l";	
	}
    else if (strcmp(paramet,"AMON")==0 || strcmp(paramet,"OXI")==0 || strcmp(paramet,"CLO")==0|| 
			strcmp(paramet,"NITRI")==0 || strcmp(paramet,"NITRA")==0 || strcmp(paramet,"SULF")==0 || 
			strcmp(paramet,"F")==0 || strcmp(paramet,"CL2LIB")==0 || strcmp(paramet,"CL2COB")==0 ){	return "mg/l";	
	}
	else if (strcmp(paramet,"C4C2")==0 || strcmp(paramet,"112TCE")==0 || strcmp(paramet,"12DCE")==0|| 
			strcmp(paramet,"12DCP")==0 || strcmp(paramet,"13DCB")==0 || strcmp(paramet,"14DCB")==0 || 
			strcmp(paramet,"BEN")==0 || strcmp(paramet,"BDCM")==0 || strcmp(paramet,"BR3CH")==0 
			|| strcmp(paramet,"C13CDP")==0 || strcmp(paramet,"CB")==0 || strcmp(paramet,"CL3CH")==0 || strcmp(paramet,"CLVI")==0 || strcmp(paramet,"ETB")==0
			|| strcmp(paramet,"PLTOT")==0 || strcmp(paramet,"THM")==0 || strcmp(paramet,"TOL")==0 || strcmp(paramet,"CLET")==0)	{	return "µg/l";	
	}
	  else if (strcmp(paramet,"AL")==0||strcmp(paramet,"AS")==0||strcmp(paramet,"BA")==0||strcmp(paramet,"B")==0||
	strcmp(paramet,"CD")==0||strcmp(paramet,"CR")==0||strcmp(paramet,"CU")==0||strcmp(paramet,"FE")==0||
	strcmp(paramet,"NI")==0||strcmp(paramet,"PB")==0||strcmp(paramet,"MN")==0||strcmp(paramet,"SB")==0||
	strcmp(paramet,"SE")==0||strcmp(paramet,"ZN")==0||strcmp(paramet,"MO")==0){
	return "µg/l";}	
	 else 	{	return "?";}
}  


char *metodo (char const *met){
	//obtiene el metodo ensayo recibiendo el parametro de analisis
	if (strcmp(met,"conduc")==0){
	return "XXBXBCXB";}
	else if (strcmp(met,"ph")==0){
	return "XXX";	
	}
	else 	{
	return "?";}
} 




int main(int argc, char *argv[]) {
	
	char * cadena;	 
	//char cadena[350];
	char * token;
	int lineas=0,numanalisis=0,cont,i,j,contcompleto=0,contgrifo=0,contsuperv=0,contlibre=0,apta=0,noapta=0;
	FILE  *muestra, *analisisekuis, *metales, *vocs, *xml;
	
	typedef struct {
		unsigned long int referencia;
		unsigned long int ekuis;
		char tipo [13];
		unsigned short int motivo;
		char fecharec [12];
		char fechaaut[19];
		unsigned short int calificacion;
    }Registro;
    
    Registro *reg;

	typedef struct{
		unsigned long int idlims;
		unsigned short int Vcoto;
		char Tcoto[1];	
		unsigned short int Vecoli;
		char Tecoli[1];
	    float Vturb;
		char Tturb[1];
		char Vamon[10];
		char Tamon[1];
		float Vph;
		char Tph[1];
		float Vsulf;
		char Tsulf[1];
		unsigned short int Vconduc;
		char Tconduc[1];
		float Vf;
		char Tf[1];
		float Vclo;
		char Tclo[1];
		float Vnitra;
		char Tnitra[1];
	    float Vnitri;
		char Tnitri[1];
		float Vcl2lib;
		char Tcl2lib[1];
		float Vcl2cob;
		char Tcl2cob[1];
		unsigned short int Ventco;
		char Tentco[1];
		unsigned short int Vbh22;
		char Tbh22[1];
		unsigned short int Vclper;
		char Tclper[1];
		float Voxi;
		char Toxi[1];		
	}Analitica;

	
	Analitica *analisis;

	printf("Ayuntamiento de Vitoria, Laboratorio Municipal\n\a ");
	
	muestra=fopen("EKUIS_muestra.csv","r");
	printf("Procesando el fichero registro.csv\n ");			
		
	if (muestra==NULL){
		printf("Error al abrir el fichero");
	}
	
   	 
	
	if((  cadena = (char*)malloc ( MAXregistro*sizeof(char) )   ) == NULL) {
        return -1;
    };
			
		  
    while( !feof(muestra) ) {
  	 fgets(cadena,MAXregistro,muestra);
  	 numanalisis++;	
  }
  
   rewind(muestra);
  
   
   // comprobar cabecera fichero registro
   
	 fgets(cadena,MAXregistro,muestra);
	   
    	token=strtok(cadena,",");
		
			if (token !=NULL){
	        cont=0;
			while (token !=NULL){
			switch (cont){
				case 0: 
				    if (strcmp(token,"ID Numeric")!=0) {printf( "Error en la lectura de cabecera registro idnum");exit(-1);}	
				    break;
				case 1: 
				   if (strcmp(token,"NumEkuis")!=0) {printf( "Error en la lectura de cabecera registro ekuis");exit(-1);}
				    break;
            	case 2: 
				    if (strcmp(token,"Tipo_Analisis")!=0) {printf( "Error en la lectura de cabecera registro tipo");exit(-1);}	
				    break;
				case 3: 
				   if (strcmp(token,"Motivo ekuis")!=0) {printf( "Error en la lectura de cabecera registtro motiv");exit(-1);}
				    break;
   	            case 4: 
				    if (strcmp(token,"SampledDate")!=0) {printf( "Error en la lectura de cabecera registro date samp");exit(-1);}	
				    break;
				case 5: 
				   if (strcmp(token,"Obser ekuis")!=0) {printf( "Error en la lectura de cabecera registro obs");exit(-1);}
				    break;
   				case 6: 
				    if (token[5]!='a' && token[6]!='u'){printf( "Error en la lectura de cabecera registro date auth");exit(-1);}
					break;					    				    
	    }
			token=strtok(NULL,",");	
			cont++;
			} 		// end of while	
	} // end del if	
		
		numanalisis--;
	printf("Cabecera de fichero EKUIS_registro correcto\n");

	
   	reg = (Registro*)malloc(numanalisis*sizeof(Registro));
    	if (reg==NULL) {printf("Error al asignar memoria");
		}
   printf("Se van a procesar %d muestras\n",numanalisis);
   
     // fgets(cadena,MAXregistro,muestra);
   
   
   
    for (i=0;!feof(muestra);i++ ){
		// funcion split para separar por comas	i es la linea	
    fgets(cadena,MAXregistro,muestra);
  
	token=strtok(cadena,",");	

//	 if (token !=NULL){
	 	cont=0;
				while (token !=NULL){
			 
			  
				//for(j=0;j<6;j++){
	
				
						switch (cont){
								case 0:  //Id numeric
								    if (token=="\0"){printf("Error. Falta un id en linea num %d",i);exit(-1);	}
								    reg[i].referencia=atol(token);	
								    break;
								case 1:   // numekuis
								    //if (token=="\0"){printf("Error. Falta un id ekuis en linea num %d",i);exit(-1);	}
								    if (token!=NULL){
									reg[i].ekuis=atol(token);} else {printf("es nulo");
									}
									
									if(token=="\0"){printf("nuloo");
									}
								    break;
							    case 2:   //tipo
								    if (token=="\0"){	token="LIBRE"	;	}
									if (strcmp(token,"COMPLETO")==0) { strcpy(reg[i].tipo,"COMPLE");contcompleto++;}
									else if (strcmp(token,"GRIFO")==0) {strcpy(reg[i].tipo,"GRIFO");contgrifo++;}
									else if (strcmp(token,"SUPERVISION")==0) {strcpy(reg[i].tipo,"SUPER");contsuperv++;}else {strcpy(reg[i].tipo,"LIBRE"); contlibre++;}
								    break; 
							    case 3:   //motivo
							        if (token=="\0"){ token="97";	}
								    reg[i].motivo=atoi(token);	
								    break;  
							    case 4:  
								    if (token=="\0"){printf("Error. Falta una fecha en linea num %d",i);exit(-1);	}
								    strcpy(reg[i].fecharec,token);	
								    break;	
							    case 5:
								   strcpy(reg[i].fechaaut,token);		
								    break; 
							    default:
								  break;      
								};
				token=strtok(NULL,",");	
				cont++;		 
				} 		// end of while	 lo cambie por for
//	} // end del if	
    	
	} // end del for
	

    printf("Se han procesado %d muestras.\n",i);
  
    for (i=0;i<10;i++){
   	printf("<muestra referencia=\"%d\" punto=\"%d\" tipo=\"%s\" motivo=\"%d\" calificacion=\"3\" fecharec=\"%s\">\n",reg[i].referencia,  reg[i].ekuis,  reg[i].tipo,  reg[i].motivo, reg[i].fecharec);
}	 		
  

  
   	fclose(muestra);
   

   
   	if((  cadena = (char*)realloc ( cadena,MAXanalitica*sizeof(char) )   ) == NULL) {
   		printf("Error al reasignar memoria.");
        return -1;
    };
    
    
    
    analisisekuis=fopen("EKUIS_analiticaBasica.csv","r");	
   	if (analisisekuis==NULL){	printf("Error al abrir el fichero\n");	}
   

   lineas=0;
 
	printf("Procesando el fichero EKUIS_analiticaBasica.csv\n ");	
	  while( !feof(analisisekuis) ) {
  	 fgets(cadena,MAXanalitica,analisisekuis);
  	 lineas++;	
  }
  rewind(analisisekuis);		
 

   printf("Se van a procesar %d test\n",lineas-1);
	
    analisis = (Analitica*)malloc(lineas*sizeof(Analitica));
   if (analisis==NULL) {printf("Error al asignar memoria");   }
    
 

//procesamiento cabecera de analitica


	    fgets(cadena,MAXanalitica,analisisekuis);
	   
		token=strtok(cadena,",");
		
			if (token !=NULL){
	        cont=0;
			while (token !=NULL){
			switch (cont){
				case 0: 
				    if (strcmp(token,"ID Numeric")!=0) {printf( "Error en la lectura de cabecera analisisbasicoid");exit(-1);}	
				    break;
				case 1: 
				if (strcmp(token,"colifTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocolt");exit(-1);}	
				    break;
			    case 2:  
				     if (strcmp(token,"colif")!=0) {printf( "Error en la lectura de cabecera analisisbasicocol");exit(-1);}
				    break; 
			    case 3:
			    	 if (strcmp(token,"ecoliTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoecolt");exit(-1);}
				    break;  
				case 4:
				    if (strcmp(token,"ecoli")!=0) {printf( "Error en la lectura de cabecera analisisbasicoecol");exit(-1);}
				    break;  
				case 5:
					 if (strcmp(token,"turbTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoturt");exit(-1);}
				    break;  
				case 6:
				    if (strcmp(token,"turb")!=0) {printf( "Error en la lectura de cabecera analisisbasicotur");exit(-1);}
				    break;  
				case 7:
					 if (strcmp(token,"amonTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoamot");exit(-1);}
				    break;  
				case 8:
				     if (strcmp(token,"amon")!=0) {printf( "Error en la lectura de cabecera analisisbasicoamo");exit(-1);}
				    break;   
				case 9:
					  if (strcmp(token,"phTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicopht");exit(-1);}
				    break;  
				case 10:
				  if (strcmp(token,"ph")!=0) {printf( "Error en la lectura de cabecera analisisbasicoph");exit(-1);}
				    break;  
				case 11:
				 if (strcmp(token,"sulfTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicosult");exit(-1);}
				    break;  
				case 12:
				    if (strcmp(token,"sulf")!=0) {printf( "Error en la lectura de cabecera analisisbasicosul");exit(-1);}
				    break;  
				case 13:
					if (strcmp(token,"condTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocont");exit(-1);}
				    break;  
				case 14:
				      if (strcmp(token,"cond")!=0) {printf( "Error en la lectura de cabecera analisisbasicocon");exit(-1);}
				    break;  
				case 15:
					  if (strcmp(token,"fluorTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoflut");exit(-1);}
				    break;  
				case 16:
				     if (strcmp(token,"fluor")!=0) {printf( "Error en la lectura de cabecera analisisbasicoflu");exit(-1);}
				    break;  
				case 17:
					 if (strcmp(token,"cloruTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclort");exit(-1);}
				    break; 
			   	case 18:
				      if (strcmp(token,"cloru")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclor");exit(-1);}
				    break; 
				case 19:
					if (strcmp(token,"nitratoTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitraT");exit(-1);}
				    break; 
				case 20:
				    if (strcmp(token,"nitrato")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitra");exit(-1);}
				    break; 
				case 21:
					if (strcmp(token,"nitritoTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitriT");exit(-1);}
				    break; 
				case 22:
				  if (strcmp(token,"nitrito")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitri");exit(-1);}
				    break; 
				case 23:
					 if (strcmp(token,"cllibTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocllibT");exit(-1);}
				    break; 
				case 24:
				    if (strcmp(token,"cllib")!=0) {printf( "Error en la lectura de cabecera analisisbasicocllib");exit(-1);}
				    break; 
				case 25:
					if (strcmp(token,"clcombTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclcomT");exit(-1);}
				    break; 
				case 26:
				    if (strcmp(token,"Clcomb")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclcom");exit(-1);}
				    break; 
				case 27:
					 if (strcmp(token,"enteroTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoentT");exit(-1);}
				    break; 
				case 28:
				    if (strcmp(token,"entero")!=0) {printf( "Error en la lectura de cabecera analisisbasicoent");exit(-1);}
				    break; 
				case 29:
					 if (strcmp(token,"aerobTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoaerT");exit(-1);}
				    break; 
				case 30:
				      if (strcmp(token,"aerob")!=0) {printf( "Error en la lectura de cabecera analisisbasicoaer");exit(-1);}
				    break; 
				case 31:
					  if (strcmp(token,"closTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclosT");exit(-1);}
				    break; 
				case 32:
				   if (strcmp(token,"clos")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclos");exit(-1);}
				    break; 	
				case 33:
					 if (strcmp(token,"oxiTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicooxiT");exit(-1);}
				    break; 
				case 34:
				     if (token[0]!='o'&&token[1]!='x') {printf( "Error en la lectura de cabecera analisisbasicooxi");exit(-1);}
				    break; 								    				    
	    }
			token=strtok(NULL,",");	
			cont++;
			} 		// end of while	
	} // end del if	
		
	printf("Cabecera analisibasico.csv correcto\n");
	
    fgets(cadena,MAXanalitica,analisisekuis);

	   for (i=0;!feof(analisisekuis);i++ ){
		// funcion split para separar por comas	i es la linea	
	    fgets(cadena,MAXanalitica,analisisekuis);
	    
		token=strtok(cadena,",");
	
		 if (token !=NULL){
		 	cont=0;
			while (token !=NULL){
			
			switch (cont){
				case 0: 	 
				    analisis[i].idlims=atol(token);	
				
				    break;
			  //coliformes
				case 1:  
				
				    strcpy(analisis[i].Tcoto,token);
			
				    break;
			    case 2:   
				     analisis[i].Vcoto=atoi(token);
				
				    break;
			   // ecoli 
			    case 3:
				      strcpy(analisis[i].Tecoli,token);                                                                  
				    break;  
				case 4:
				      analisis[i].Vecoli=atoi(token);
				      if (analisis[i].Vecoli>0)	{ for (j=0;j<2000;j++){ if (reg[j].referencia==analisis[i].idlims){reg[j].calificacion=2;}}}
				    break;  
				 // turbidez   
				 case 5:
				     strcpy(analisis[i].Tturb,token);                                                          
				   break;  
				case 6:
				  analisis[i].Vturb=atof(token);
				    break;  
				  //amonio    
			    case 7:
				     strcpy(analisis[i].Tamon,token);  
				    break;  
				case 8:
				    strcpy(analisis[i].Vamon,token);
				    break;  
				        // ph
				case 9:  
			
				    strcpy(analisis[i].Tph,token);
			
				    break;
			    case 10:   
				    analisis[i].Vph=atof(token);
					
				    break;
				    
				    // sulfatos
				case 11:  
			
				    strcpy(analisis[i].Tsulf,token);
			
				    break;
			    case 12:   
				    analisis[i].Vsulf=atof(token);
						
				    break;				    
				    // conductividad
				    
				case 13:  
			
				    strcpy(analisis[i].Tconduc,token);
			
				    break;
			    case 14:   
				    analisis[i].Vconduc=atoi(token);
						
				    break;				    
				    // fluoruros
				    
				case 15:  
			
				    strcpy(analisis[i].Tf,token);
			
				    break;
			    case 16:   
				    analisis[i].Vf=atof(token);
						
				    break;		    
				    
				    // cloruros
				   
				case 17:  
			
				    strcpy(analisis[i].Tclo,token);
			
				    break;
			    case 18:   
				    analisis[i].Vclo=atof(token);
						
				    break;	   
				    
				    // nitrato
				    
				case 19:  
			
				    strcpy(analisis[i].Tnitra,token);
			
				    break;
			    case 20:   
				    analisis[i].Vnitra=atof(token);
						
				    break;		    
				    
				    // nitrito
				    
				case 21:  
			
				    strcpy(analisis[i].Tnitri,token);
			
				    break;
			    case 22:   
				    analisis[i].Vnitri=atof(token);
;
						
				    break;	  
				    
				    // cloro libre
				    
				case 23:  
			
				    strcpy(analisis[i].Tcl2lib,token);
			
				    break;
			    case 24:   
				    analisis[i].Vcl2lib=atof(token);
						
				    break;	   
				    
				    // cloro comb
				    
				case 25:  
			
				    strcpy(analisis[i].Tcl2cob,token);
			
				    break;
			    case 26:   
				    analisis[i].Vcl2cob=atof(token);
						
				    break;	 
				    
				    // enteros
				    
				case 27:  
			
				    strcpy(analisis[i].Tentco,token);
			
				    break;
			    case 28:   
				    analisis[i].Ventco=atoi(token);
						
				    break;	  
				    
				    // aerobios
				    
				case 29:  
			
				    strcpy(analisis[i].Tbh22,token);
			
				    break;
			    case 30:   
				    analisis[i].Vbh22=atof(token);
				
				    break;	    
				    
				    // clostridios
				    
				case 31:  
			
				    strcpy(analisis[i].Tclper,token);
			
				    break;
			    case 32:   
				    analisis[i].Vclper=atof(token);
						
				    break;		    
				    
				    // oxidabilidad  
				
				case 33:  
			
				    strcpy(analisis[i].Toxi,token);
			
				    break;
			    case 34:   
				    analisis[i].Voxi=atof(token);
						
				    break;	       	    
	    }
			token=strtok(NULL,",");	
			cont++;		 
			} 		// end of while	
	} // end del if					
}// end of for
	
	

	fclose(analisisekuis);
    printf("Se han procesado %d analisis.\n",i);
    

    
    // comienzo del xml
  
    printf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    printf("<laboratorio codigo=\"DEMSAC\">\n");
    
        for (i=0;i<10;i++){
        	
   	printf("<muestra referencia=\"%d\" punto=\"%d\" tipo=\"%s\" motivo=\"%d\" calificacion=\"%d\" fecharec=\"%s\">\n",reg[i].referencia,  reg[i].ekuis,  reg[i].tipo,  reg[i].motivo,reg[i].calificacion, reg[i].fecharec);
   	
   	
   	
   	
   	
   	  for (j=0;j<120;j++){
	
           if ( analisis[j].idlims==reg[i].referencia!=0){
           	
	           if (strcmp(analisis[j].Tconduc,"E")!=0){
            	printf("<parametro codigo=\"CONDUC\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"%cS/cm\"/>\n",analisis[j].Tconduc,analisis[j].Vconduc,230);
       			}
	  	 
		   	   if (strcmp(analisis[j].Tph,"E")!=0){
            	printf("<parametro codigo=\"PH\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"%cS/cm\"/>\n",analisis[j].Tph,analisis[j].Vph,230);
       			}
		   
		   	   if (strcmp(analisis[j].Tsulf,"E")!=0){
            	printf("<parametro codigo=\"SULF\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"%cS/cm\"/>\n",analisis[j].Tsulf,analisis[j].Vsulf,230);
       			}
	  	 
		   	   if (strcmp(analisis[j].Tf,"E")!=0){
            	printf("<parametro codigo=\"F\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"%cS/cm\"/>\n",analisis[j].Tf,analisis[j].Vf,230);
       			}
	  	 
		   	   if (strcmp(analisis[j].Tclo,"E")!=0){
            	printf("<parametro codigo=\"CLO\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"%cS/cm\"/>\n",analisis[j].Tclo,analisis[j].Vclo,230);
       			}
		   
		   	    if (strcmp(analisis[j].Tnitra,"E")!=0){
            	printf("<parametro codigo=\"NITRA\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tnitra,analisis[j].Vnitra);
       			}
	  	 
		   	   if (strcmp(analisis[j].Tnitri,"E")!=0){
            	printf("<parametro codigo=\"NITRI\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tnitri,analisis[j].Vnitri);
       			}
	  	 
		   		if (strcmp(analisis[j].Tturb,"E")!=0){
            	printf("<parametro codigo=\"TURB\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tturb,analisis[j].Vturb);
       			}
	  	 	    if (strcmp(analisis[j].Tamon,"E")!=0){
            	printf("<parametro codigo=\"AMON\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tamon,analisis[j].Vamon);
       			}
	  	 	    if (strcmp(analisis[j].Tcl2lib,"E")!=0){
            	printf("<parametro codigo=\"CL2LIB\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tcl2lib,analisis[j].Vcl2lib);
       			}
	  	 
		   	    if (strcmp(analisis[j].Tcl2cob,"E")!=0){
            	printf("<parametro codigo=\"CL2COB\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tcl2cob,analisis[j].Vcl2cob);
       			}
	
		   		if (strcmp(analisis[j].Toxi,"E")!=0){
            	printf("<parametro codigo=\"OXI\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Toxi,analisis[j].Voxi);
       			}
	  	 	    if (strcmp(analisis[j].Tcoto,"E")!=0){
            	printf("<parametro codigo=\"COTO\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tcoto,analisis[j].Vcoto);
       			}
	  	 	    if (strcmp(analisis[j].Tecoli,"E")!=0){
            	printf("<parametro codigo=\"ECOLI\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tecoli,analisis[j].Vecoli);
       			}
	  	 	    if (strcmp(analisis[j].Tentco,"E")!=0){
            	printf("<parametro codigo=\"ENTCO\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tentco,analisis[j].Ventco);
       			}
	  	 		if (strcmp(analisis[j].Tbh22,"E")!=0){
            	printf("<parametro codigo=\"BH22\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tbh22,analisis[j].Vbh22);
       			}
	  	 	   if (strcmp(analisis[j].Tclper,"E")!=0){
            	printf("<parametro codigo=\"CLPER\" ensayo=\"xxx\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"S/cm\"/>\n",analisis[j].Tclper,analisis[j].Vclper);
       			}
	  	 
	  	 
		      } 
	    
	   
	   
	   
	   
	   
	   } //end del for de cada muestra
	
    
   	
   	
   	printf("</muestra>\n");
   	
   	
   	
   	
   	
   	
}	 // fin del archivo xml		
  
    
    printf("</laboratorio>");
    
for (i=0;i<numanalisis;i++){
	
	if (reg[i].calificacion==1){ apta++;
	}else if (reg[i].calificacion==2){noapta++;
	}
	
	
}

 printf("\nResultado calificacion de potabilidad: APTAS: %d   NO APTAS: %d",apta,noapta);
	//reg[1].calificacion=2;
 
	//printf("calificacion %d ",reg[1].calificacion);

	printf("\nSe han generado las siguientes muestras: ");
	for (i=0;i<numanalisis;i++){
	printf(" %d ,",reg[i].referencia);
	}
	printf("\n");
	printf("Resumen: Grifo: %d  Supervision: %d  completos: %d  libres: %d  Total analisis: %d\n",contgrifo,contsuperv,contcompleto,contlibre,numanalisis );
	printf(" Fecha inicial: %s   Fecha final:%s \n",reg[0].fecharec,reg[i-1].fecharec );
	
	
	
	    time_t tiempo = time(0);
        struct tm *tlocal = localtime(&tiempo);
        char output[128];
        strftime(output,128,"%d/%m/%y %H:%M:%S",tlocal);
        printf("Fecha actual: %s\n",output);

/*	
	if (fclose(xml)==0){
		printf("\nSe cerro el fichero correctamente.\n");
	   	printf("Ayuntamiento de Vitoria, Laboratorio Municipal\n ");
	}
	
	*/
	//free(cadena);

	return 0;
} // end of main

