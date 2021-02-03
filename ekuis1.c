/**
* @file ekuis.c
* @version 1.0
* @Author Oscar Unzueta
* @date 29/11/2020
* @brief generacion archivo xml para exportar datos analiticos del lims al EKUIS, crea la calificacion de muestra y reporte txt
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAXanalitica 400
#define MAXregistro 290
#define MAXmetales 200
#define MAXVOC1 190
#define MAXVOC2 115


void PrintFloat(float valor,int precision,char *Buffer)
{
   int c;
   char caracter;
   int bufferPos=0;
   long parteEntera,parteDecimal;
   float mul10=1;
   for(c=0;c<precision;c++)
      mul10*=10;
   parteEntera=abs((long)valor);
   parteDecimal=abs((long)((valor-(long)valor)*mul10));
   do
   {
      Buffer[bufferPos]=(char)(parteEntera%10)+'0'; bufferPos++;
      parteEntera/=10;
   }
   while(parteEntera>0);
   if(valor<0)
   {
      Buffer[bufferPos]='-'; bufferPos++;
   }
   for(c=0;c<bufferPos/2;c++)
   {
      caracter=Buffer[c]; Buffer[c]=Buffer[bufferPos-c-1]; Buffer[bufferPos-c-1]=caracter;
   }
   if(precision>0)
   {
      Buffer[bufferPos]='.'; bufferPos++;
      int parteDecimalPos=bufferPos;
      for(c=0;c<precision;c++)
      {
         Buffer[bufferPos]=(char)(parteDecimal%10)+'0'; bufferPos++;
         parteDecimal/=10;
      }
      for(c=0;c<precision/2;c++)
      {
         caracter=Buffer[c+parteDecimalPos]; 
         Buffer[c+parteDecimalPos]=Buffer[bufferPos-c-1]; 
         Buffer[bufferPos-c-1]=caracter;
      }
   }
   Buffer[bufferPos]=0;
}


char *simbolo (char const *tipo){
		if (strcmp(tipo,"N")==0){	return " ";	
	}
	  else if (strcmp(tipo,"L")==0){	return "<";	}else if (strcmp(tipo,"G")==0){ return ">";}
	
}


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
	char aux;
	char dia[2];
	char mes[2];
	char ano [4];
    char hoy[128];	 
    char texto[100];
    char ruta[80];
	char pBuffer[15];
	char * token;
	char * buf;
	char temp[19]; 
	char incumplimiento[100];
	char miniincumplimiento[100];
	int lineas=0,numanalisis=0,numanalisismetales=0,numanalisisbasicos=0,numanalisisvoc1=0,numanalisisvoc2=0,cont,i,j,k,contcompleto=0,contgrifo=0,contsuperv=0,
	contlibre=0,apta=0,noapta=0,sincalif=0,cumple=0,max=0,min=0,lims,cali,opcion;
    int flag;
	FILE  *muestra, *analisisekuis, *metales, *voc1, *voc2, *xml, *reporteEkuis;
	
	typedef struct {
		unsigned long int referencia;
		unsigned long int ekuis;
		char tipo [13];
		unsigned short int motivo;
		char fecharec [12];
		char obs[100];
		char obscali[150];
		char fechaaut[19];
		char nombre[50];
		unsigned short int calificacion;
		char fraseincumplimientoBas[80];
		char fraseincumplimientoMet[80];
		char fraseincumplimientoVoc1[80];
		char fraseincumplimientoVoc2[80];
		char miniincumplimientos[90];
    }Registro;
    
    Registro *reg;

	typedef struct{
		unsigned long int idlims;
		unsigned short int Vcoto;
		char Tcoto[1];	
	    int Vecoli;
		char Tecoli[1];
	    float Vturb;
		char Tturb[2];
		float Vamon;
		char Tamon[2];
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
	
	
	typedef struct {
		unsigned long int idlims;
		char Tal[1];
		float Val;
		char Tsb[1];
		float Vsb;	
		char Tas[1];
		float Vas;
		char Tba[1];
		float Vba;
		char Tb[1];
		float Vb;
		char Tcd[1];
		float Vcd;
		char Tcu[1];
		float Vcu;
		char Tcr[1];
		float Vcr;
		char Tfe[1];
		float Vfe;
		char Tmn[1];
		float Vmn;
		char Tmo[1];
		float Vmo;
		char Tni[1];
		float Vni;
		char Tpb[1];
		float Vpb;
		char Tse[1];
		float Vse;
		char Tzn[1];
		float Vzn;
	}Metales;

    Metales *Analisismetales;
    
    
    typedef struct{
        unsigned long int idlims;
		char Tald[1];
		float Vald;	
		char Tc4c2[1];
		float Vc4c2;	
    	char T112tce[1];
		float V112tce;
		char T12dce[1];
		float V12dce;
		char T12dcp[1];
		float V12dcp;
		char T13dcb[1];
		float V13dcb;
		char T14dcb[1];
		float V14dcb;
		char Tben[1];
		float Vben;
		char Tbdcm[1];
		float Vbdcm;
		char Tbr3ch[1];
		float Vbr3ch;
		char Tc13dcp[1];
		float Vc13dcp;
		char Tcb[1];
		float Vcb;
		char Tcl3ch[1];
		float Vcl3ch;
		char Tclvi[1];
		float Vclvi;
		char Tdiel[1];
		float Vdiel;
		char Tetb[1];
		float Vetb;
		char Thpcl[1];
		float Vhpcl;
		char Thpcle[1];
		float Vhpcle;
		char Tpltot[1];
		float Vpltot;
		char Tthm[1];
		float Vthm;
		char Ttol[1];
		float Vtol;
		char Tclet[1];
		float Vclet;
		
	}VOCS;
    
    
    VOCS *voc;
    
    
    typedef struct {
    unsigned short int dia;
	unsigned short int mes;
	unsigned short int ano;	
	}Fecha;
	
	Fecha *fechaaut;
	printf("Ayuntamiento de Vitoria, Laboratorio Municipal\n\a ");
	muestra=fopen("EKUIS_muestra.csv","r");
	printf("\nProcesando el fichero registro.csv\n ");			
		
	if (muestra==NULL){
		printf("\nError al abrir el fichero muestra.csv");
	}
	
	
	if((  cadena = (char*)malloc ( MAXregistro*sizeof(char) )   ) == NULL) {
        return -1;
    }
			
		  
   while( !feof(muestra) ) {
     strcpy(texto,cadena);
     fgets(cadena,MAXregistro,muestra); 
	
     if(strcmp(cadena,texto)!=0) numanalisis++;
  	  
  	  //para no contar la ultima linea si solo tiene salto de linea ya que da error y se repite al final
  	
    }
    numanalisis--;  //descontar la cabecera

  
  // lee uno de mas, la cabecera
  
 


   rewind(muestra);
  
   
   // comprobar cabecera fichero registro
   
	 fgets(cadena,MAXregistro,muestra);
	   
     token=strtok(cadena,",");
		
		if (token !=NULL){
	        cont=0;
			while (token !=NULL){
			switch (cont){
				case 0: 
				    if (strcmp(token,"ID Numeric")!=0) {printf( "Error en la lectura de cabecera registro idnum");getchar();getchar();exit(-1);}	
				    break;
				case 1: 
				   if (strcmp(token,"NumEkuis")!=0) {printf( "Error en la lectura de cabecera registro ekuis");getchar();getchar();exit(-1);}
				    break;
            	case 2: 
				    if (strcmp(token,"Tipo_Analisis")!=0) {printf( "Error en la lectura de cabecera registro tipo");getchar();getchar();exit(-1);}	
				    break;
				case 3: 
				   if (strcmp(token,"Motivo ekuis")!=0) {printf( "Error en la lectura de cabecera registtro motiv");getchar();getchar();exit(-1);}
				    break;
   	            case 4: 
				    if (strcmp(token,"SampledDate")!=0) {printf( "Error en la lectura de cabecera registro date samp");getchar();getchar();exit(-1);}	
				    break;
				case 5: 
				   if (strcmp(token,"Obser ekuis")!=0) {printf( "Error en la lectura de cabecera registro obs");getchar();getchar();exit(-1);}
				    break;
				 
				case 6:    
				     if (strcmp(token,"Nombre")!=0) {printf( "Error en la lectura de cabecera registro nombre");getchar();getchar();exit(-1);}
				    break;
				       
   				case 7: 
				    if (token[5]!='a' && token[6]!='u'){printf( "Error en la lectura de cabecera registro date auth");getchar();getchar();exit(-1);}
					break;					    				    
	    }
			token=strtok(NULL,",");	
			cont++;
			} 		// end of while	
	} // end del if	
		
	
	printf("\nCabecera de fichero EKUIS_registro correcto\n");

	
   	reg = (Registro*)malloc(numanalisis*sizeof(Registro));
    	if (reg==NULL) {printf("\nError al asignar memoria registro");
		}
    printf("\nSe van a procesar %d muestras\n",numanalisis);
   
   	fechaaut = (Fecha*)malloc(numanalisis*sizeof(Fecha));
   	if (reg==NULL) {printf("\nError al asignar memoria fecha");
		}


// meter datos de analitica basica


   //for(i=0;!feof(muestra);i++){ // iterador general
for(i=0;i<numanalisis;i++){ // iterador general      
		fgets(cadena,MAXregistro,muestra);   
	
       	for (j=0;j<19;j++){	 temp[j]='\0'; }	 
        cont=0;
		aux ='0';
	  
	  
	  // columna 0
	  
      for (j=0;aux!=',';j++){	
	    aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

	
	if (strcmp(temp,"\0")==0){printf("Error. Falta un idlims en linea num %d\n",i);exit(-1);}else {reg[i].referencia=atol(temp);	}
	 
	// columna 1
	
	aux=cadena[cont];

	for (j=0;j<19;j++){	 temp[j]='\0'; }
	
    for (j=0;aux!=',';j++){
	     aux=cadena[cont];	    cont++;
	     if (aux!=','){temp[j]=aux;     }
     }


	if (strcmp(temp,"\0")==0)
	{printf("Error. Falta un idekuis en lims num %d\n",reg[i].referencia);cont++;}else {reg[i].ekuis=atol(temp);	}

  // columna 2   Tipo
  
   aux=cadena[cont];	

	for (j=0;j<19;j++){	 temp[j]='\0'; }
	
    for (j=0;aux!=',';j++){
    
	      aux=cadena[cont];    cont++;
	      if (aux!=','){temp[j]=aux;     }
     }

     
   	if (strcmp(temp,"COMPLETO")==0) { strcpy(reg[i].tipo,"COMPLE");contcompleto++;}
									else if (strcmp(temp,"GRIFO")==0) {strcpy(reg[i].tipo,"GRIFO");contgrifo++;}
									else if (strcmp(temp,"SUPERVISION")==0) {strcpy(reg[i].tipo,"SUPER");contsuperv++;}
									else {strcpy(reg[i].tipo,"LIBRE"); contlibre++;cont++;}
	
	
	// columna 3 Motivo
	
	  aux=cadena[cont];	
	
	  for (j=0;j<19;j++){	 temp[j]='\0'; }
	
	  for (j=0;aux!=',';j++){	
	    aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

	
	if (strcmp(temp,"\0")==0){//si no hay motivo se le asigna 3 que es vigilancia sanitaria
	  reg[i].motivo=3;cont++;
	    }else {reg[i].motivo=atoi(temp);	}
	 
	
	// columna 4  fecha recogida
	 
	aux=cadena[cont];	
	
	for (j=0;j<19;j++){temp[j]='\0'; }
	
	
	for (j=0;aux!=',';j++){	
	    aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
     }


	if (strcmp(temp,"\0")==0){
	   printf("\nError. Falta una fecha recogida en lims num %d\n",reg[i].referencia);cont++;
	  }else {  strcpy(reg[i].fecharec,temp);
	} 
	 
	//columna 5
	
	 aux=cadena[cont];	
	
     for (j=0;j<19;j++){	 temp[j]='\0'; }
	
	
	 for (j=0;aux!=',';j++){	
	    aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

   	 if (strcmp(temp,"\0")==0){cont++; strcpy(reg[i].obs," "); }  else {  strcpy(reg[i].obs,temp);	}  // si obs está vacio poner cadena cvacia
	
	
    // columna 6

     aux=cadena[cont];	
	
     for (j=0;j<40;j++){	 temp[j]='\0'; }
	
	
	 for (j=0;aux!=',';j++){	
	    aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

   	 if (strcmp(temp,"\0")==0){cont++; strcpy(reg[i].nombre,"\0"); }  else {  strcpy(reg[i].nombre,temp); printf("********%s\n",temp);	}  // si nombre está vacio poner cadena cvacia

	
	//columna 7
	
   	for (j=0;j<19;j++){	 temp[j]='\0'; }
	
	
	for (j=0;j<18;j++){
		aux=cadena[cont];  cont++;
		if (aux!=','){temp[j]=aux;  }
     }

    
	if (strcmp(temp,"\0")==0){printf("\nError. Falta un fecha auth en lims num %d  \n",reg[i].referencia);}else {  strcpy(reg[i].fechaaut,strtok(temp," "));	} // elimino la hora en fecha autorizada
	
	
// control de fecha autorizada	
  
	dia[0]=temp[0];
	dia[1]=temp[1];
	mes[0]=temp[3];
	mes[1]=temp[4];
	ano[0]=temp[6];
	ano[1]=temp[7];
	ano[2]=temp[8];
	ano[3]=temp[9];
	
	
    fechaaut[i].dia=atoi(dia);
    fechaaut[i].mes=atoi(mes);
	fechaaut[i].ano=atoi(ano);
	
	reg[i].calificacion=1;  //inicializa calificacion a 1
}



    printf("\nSe han procesado %d muestras de analisis basico.\n",numanalisis);

  
   	fclose(muestra);
   
// calculo fecha autorizada maxima y minima
for (i=0;i<numanalisis-1;i++){
	
   if (fechaaut[i].dia+fechaaut[i].mes*30+fechaaut[i].ano*365>max){max=i;
   }  
   if (fechaaut[i].dia+fechaaut[i].mes*30+fechaaut[i].ano*365<min){min=i;
   }
}

		

	// crear archivo reporte ekuis.txt
	strcpy(ruta,"reporteEkuis");

	strcpy(texto,reg[min].fechaaut);
	token=texto;

     for (j=0;j<7;j++){
     	
     if(	*(token+j)=='/') {
	 *(token+j)='-';
	 }}

	strcat(ruta,token);
	strcat(ruta,"_");
	strcpy(texto,reg[max].fechaaut);

	token=texto;

     for (j=0;j<7;j++){
     	
     if(	*(token+j)=='/') {
	 *(token+j)='-';
	 }}
	 
	 strcat(ruta,token);
	 strcat(ruta,".txt");
     reporteEkuis=fopen(ruta,"w");
   
if (reporteEkuis==NULL){printf("Error al escribir el reporte\n");
}


   
   	if((  cadena = (char*)realloc ( cadena,MAXanalitica*sizeof(char) )   ) == NULL) {
   		printf("\nError al reasignar memoria analitica basica.");
        return -1;
    };
    
    
    analisisekuis=fopen("EKUIS_analiticaBasica.csv","r");	
   	if (analisisekuis==NULL){	printf("Error al abrir el fichero\n");	}

    lineas=0;
 
	printf("Procesando el fichero EKUIS_analiticaBasica.csv\n ");
	
    while( !feof(analisisekuis) ) {
  	 fgets(cadena,MAXanalitica,analisisekuis);
  	 lineas++;	   // habra que eliminar lineas ahora que llevo la cuenta general con numanalisis
     }
    
    
    		
    rewind(analisisekuis);

    printf("Se van a procesar %d test\n",numanalisis);
	
    analisis = (Analitica*)malloc(lineas*sizeof(Analitica));
    if (analisis==NULL) {printf("\nError al asignar memoria");   }
    
 

//procesamiento cabecera de analitica


	    fgets(cadena,MAXanalitica,analisisekuis);
	   
		token=strtok(cadena,",");
		
			if (token !=NULL){
	        cont=0;
			while (token !=NULL){
			switch (cont){
				case 0: 
				    if (strcmp(token,"ID Numeric")!=0) {printf( "Error en la lectura de cabecera analisisbasicoid");getchar();getchar();exit(-1);}	
				    break;
				case 1: 
				if (strcmp(token,"colifTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocolt");getchar();getchar();exit(-1);}	
				    break;
			    case 2:  
				     if (strcmp(token,"colif")!=0) {printf( "Error en la lectura de cabecera analisisbasicocol");getchar();getchar();exit(-1);}
				    break; 
			    case 3:
			    	 if (strcmp(token,"ecoliTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoecolt");getchar();getchar();exit(-1);}
				    break;  
				case 4:
				    if (strcmp(token,"ecoli")!=0) {printf( "Error en la lectura de cabecera analisisbasicoecol");getchar();getchar();exit(-1);}
				    break;  
				case 5:
					 if (strcmp(token,"turbTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoturt");getchar();getchar();exit(-1);}
				    break;  
				case 6:
				    if (strcmp(token,"turb")!=0) {printf( "Error en la lectura de cabecera analisisbasicotur");getchar();getchar();exit(-1);}
				    break;  
				case 7:
					 if (strcmp(token,"amonTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoamot");getchar();getchar();exit(-1);}
				    break;  
				case 8:
				     if (strcmp(token,"amon")!=0) {printf( "Error en la lectura de cabecera analisisbasicoamo");getchar();getchar();exit(-1);}
				    break;   
				case 9:
					  if (strcmp(token,"phTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicopht");getchar();getchar();exit(-1);}
				    break;  
				case 10:
				  if (strcmp(token,"ph")!=0) {printf( "Error en la lectura de cabecera analisisbasicoph");getchar();getchar();exit(-1);}
				    break;  
				case 11:
				 if (strcmp(token,"sulfTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicosult");getchar();getchar();exit(-1);}
				    break;  
				case 12:
				    if (strcmp(token,"sulf")!=0) {printf( "Error en la lectura de cabecera analisisbasicosul");getchar();getchar();exit(-1);}
				    break;  
				case 13:
					if (strcmp(token,"condTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocont");getchar();getchar();exit(-1);}
				    break;  
				case 14:
				      if (strcmp(token,"cond")!=0) {printf( "Error en la lectura de cabecera analisisbasicocon");getchar();getchar();exit(-1);}
				    break;  
				case 15:
					  if (strcmp(token,"fluorTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoflut");getchar();getchar();exit(-1);}
				    break;  
				case 16:
				     if (strcmp(token,"fluor")!=0) {printf( "Error en la lectura de cabecera analisisbasicoflu");getchar();getchar();exit(-1);}
				    break;  
				case 17:
					 if (strcmp(token,"cloruTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclort");getchar();getchar();exit(-1);}
				    break; 
			   	case 18:
				      if (strcmp(token,"cloru")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclor");getchar();getchar();exit(-1);}
				    break; 
				case 19:
					if (strcmp(token,"nitratoTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitraT");getchar();getchar();exit(-1);}
				    break; 
				case 20:
				    if (strcmp(token,"nitrato")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitra");getchar();getchar();exit(-1);}
				    break; 
				case 21:
					if (strcmp(token,"nitritoTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitriT");getchar();getchar();exit(-1);}
				    break; 
				case 22:
				  if (strcmp(token,"nitrito")!=0) {printf( "Error en la lectura de cabecera analisisbasiconitri");getchar();getchar();exit(-1);}
				    break; 
				case 23:
					 if (strcmp(token,"cllibTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicocllibT");getchar();getchar();exit(-1);}
				    break; 
				case 24:
				    if (strcmp(token,"cllib")!=0) {printf( "Error en la lectura de cabecera analisisbasicocllib");getchar();getchar();exit(-1);}
				    break; 
				case 25:
					if (strcmp(token,"clcombTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclcomT");getchar();getchar();exit(-1);}
				    break; 
				case 26:
				    if (strcmp(token,"Clcomb")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclcom");getchar();getchar();exit(-1);}
				    break; 
				case 27:
					 if (strcmp(token,"enteroTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoentT");getchar();getchar();exit(-1);}
				    break; 
				case 28:
				    if (strcmp(token,"entero")!=0) {printf( "Error en la lectura de cabecera analisisbasicoent");getchar();getchar();exit(-1);}
				    break; 
				case 29:
					 if (strcmp(token,"aerobTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoaerT");getchar();getchar();exit(-1);}
				    break; 
				case 30:
				      if (strcmp(token,"aerob")!=0) {printf( "Error en la lectura de cabecera analisisbasicoaer");getchar();getchar();exit(-1);}
				    break; 
				case 31:
					  if (strcmp(token,"closTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclosT");getchar();getchar();exit(-1);}
				    break; 
				case 32:
				   if (strcmp(token,"clos")!=0) {printf( "Error en la lectura de cabecera analisisbasicoclos");getchar();getchar();exit(-1);}
				    break; 	
				case 33:
					 if (strcmp(token,"oxiTipo")!=0) {printf( "Error en la lectura de cabecera analisisbasicooxiT");getchar();getchar();exit(-1);}
				    break; 
				case 34:
				     if (token[0]!='o' && token[1]!='x') {printf( "Error en la lectura de cabecera analisisbasicooxi");getchar();getchar();exit(-1);}
				    break; 								    				    
	    }
			token=strtok(NULL,",");	
			cont++;
			} 		// end of while	
	} // end del if	
		
	printf("Cabecera analisibasico.csv correcto\n");
	
   
 
   // procesar analisis ekuis


   strcpy(incumplimiento,"\0");
   strcpy(miniincumplimiento,"\0");
   
// for(i=0;!feof(analisisekuis);i++){ // iterador general
  for(i=0;i<numanalisis;i++){ // iterador general             
     strcpy(incumplimiento,"\0");
     strcpy(miniincumplimiento,"\0");
     strcpy(cadena,"\0");
	 fgets(cadena,MAXanalitica,analisisekuis); 

     for (j=0;j<19;j++){	 temp[j]='\0'; }	 
     cont=0;
	 aux ='0';
	  
	  
	// columna idlims
	  
      for (j=0;aux!=',';j++){ aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }


	if (strcmp(temp,"\0")==0){printf("Error. Falta un idlims en linea num %d\n",i);exit(-1);}else {	analisis[i].idlims=atol(temp);	}
	 
	// columna Tcoto
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tcoto,"E");cont++;}else {  strcpy(analisis[i].Tcoto,temp);	}


	// columna Vcoto
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tcoto,"E")==0){cont++;}else {  analisis[i].Vcoto=atol(temp);	}
    if ( strcmp(analisis[i].Tcoto,"E")!=0 ) {  if ( analisis[i].Vcoto>0 && analisis[i].Vcoto<10) { PrintFloat( analisis[i].Vcoto,0,pBuffer);strcat(miniincumplimiento," Coliformes totales "); strcat(miniincumplimiento,pBuffer);	}    }
    if (strcmp(analisis[i].Tcoto,"E")!=0) {if ( analisis[i].Vcoto>10) { PrintFloat( analisis[i].Vcoto,0,pBuffer);strcat(incumplimiento," *COLIFORMES TOTALES "); strcat(incumplimiento,pBuffer);	} }
   	
	// columna Tecoli
    aux=cadena[cont];	
	for (j=0;j<5;j++){	 temp[j]='\0'; } 
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 ){strcpy(analisis[i].Tecoli,"E");cont++;}else {  strcpy(analisis[i].Tecoli,temp);	}

	// columna Vecoli
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tecoli,"E")==0){cont++;}else {  analisis[i].Vecoli=atoi(temp);	}
    if ( strcmp(analisis[i].Tecoli,"E")!=0) {if ( analisis[i].Vecoli>0) {PrintFloat( analisis[i].Vecoli,0,pBuffer);strcat(incumplimiento," *ECOLI ");strcat(incumplimiento,pBuffer);	}}
	
	// columna Tturb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tturb,"E");cont++;}else {  strcpy(analisis[i].Tturb,temp);	}
   

	// columna Vturb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tturb,"E")==0){cont++;}else {  analisis[i].Vturb=atof(temp);     	}
    if ( strcmp(analisis[i].Tturb,"E")!=0 ) { if ( analisis[i].Vturb>5) {PrintFloat( analisis[i].Vturb,1,pBuffer);strcat(incumplimiento," *TURBIDEZ ");strcat(incumplimiento,pBuffer);	}}
    
	// columna Tamon
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tamon,"E");cont++;}else {  strcpy(analisis[i].Tamon,temp);	}

	// columna Vamon
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tamon,"E")==0){cont++;}else {  analisis[i].Vamon=atof(temp);	}
    if ( strcmp(analisis[i].Tamon,"E")!=0 ) { if ( analisis[i].Vamon>0.21) { PrintFloat( analisis[i].Vamon,0,pBuffer);strcat(incumplimiento," *AMONIO ");strcat(incumplimiento,pBuffer);	}}
  
	// columna Tph
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tph,"E");cont++;}else {  strcpy(analisis[i].Tph,temp);	}

	// columna Vph

	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tph,"E")==0){cont++;}else {  analisis[i].Vph=atof(temp);	}
   	if (strcmp(temp,"\0")!=0 && strcmp(temp,"E")!=0 ){  
    if ( analisis[i].Vph>9.5) {PrintFloat( analisis[i].Vph,2,pBuffer);strcat(miniincumplimiento," PH alto");strcat(miniincumplimiento,pBuffer);}
    if ( analisis[i].Vph<6.5) {PrintFloat( analisis[i].Vph,2,pBuffer);strcat(miniincumplimiento," PH bajo ");strcat(miniincumplimiento,pBuffer);	}}
   
	// columna Tsulf
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tsulf,"E");cont++;}else {  strcpy(analisis[i].Tsulf,temp);	}
    
	// columna Vsulf
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tsulf,"E")==0){cont++;}else {  analisis[i].Vsulf=atof(temp);	}
    if ( strcmp(analisis[i].Tsulf,"E")!=0 ) {   if ( analisis[i].Vsulf>250) {PrintFloat( analisis[i].Vsulf,1,pBuffer);strcat(miniincumplimiento," Sulfato ");strcat(miniincumplimiento,pBuffer)	;	}  }   
   	
	// columna Tconduc
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tconduc,"E");cont++;}else {  strcpy(analisis[i].Tconduc,temp);	}

	// columna Vconduc
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tconduc,"E")==0){cont++;}else {  analisis[i].Vconduc=atoi(temp);	}	  
    if (strcmp(analisis[i].Tconduc,"E")!=0 ) {   if ( analisis[i].Vconduc>2500) {PrintFloat( analisis[i].Vconduc,1,pBuffer);strcat(miniincumplimiento," Conductividad ");strcat(miniincumplimiento,pBuffer);	} else if	
	( analisis[i].Vconduc<50){printf("\nconductividad muy baja, quizás sea un blanco??? lims  %d\n",analisis[i].idlims); fprintf(reporteEkuis,"\nconductividad muy baja, quizás sea un blanco??? lims %d\n",analisis[i].idlims);
	} }
	
	// columna Tf
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tf,"E");cont++;}else {  strcpy(analisis[i].Tf,temp);	}


	// columna Vf
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tf,"E")==0){cont++;}else {  analisis[i].Vf=atof(temp);	}
    if (strcmp(analisis[i].Tf,"E")!=0 ) {   if ( analisis[i].Vf>0.8) {PrintFloat( analisis[i].Vf,1,pBuffer);strcat(incumplimiento," *Fluor ");strcat(incumplimiento,pBuffer);	}  }
	
	// columna Tclo
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tclo,"E");cont++;}else {  strcpy(analisis[i].Tclo,temp);	}


	// columna Vclo
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tclo,"E")==0){cont++;}else {  analisis[i].Vclo=atof(temp);	}
    if (strcmp(analisis[i].Tclo,"L")!=0  && strcmp(analisis[i].Tclo,"E")!=0 ) {    if ( analisis[i].Vclo>250) {PrintFloat( analisis[i].Vclo,1,pBuffer);strcat(miniincumplimiento," Cloruro ");strcat(miniincumplimiento,pBuffer);	} }
	
	// columna Tnitra
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tnitra,"E");cont++;}else {  strcpy(analisis[i].Tnitra,temp);	}


	// columna Vnitra
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tnitra,"E")==0){cont++;}else {  analisis[i].Vnitra=atof(temp);	}
    if (  strcmp(analisis[i].Tnitra,"E")!=0 ) {  if ( analisis[i].Vnitra>50) {PrintFloat( analisis[i].Vnitra,1,pBuffer);strcat(incumplimiento," *NITRATO ");strcat(incumplimiento,pBuffer);	}  }
    
	// columna Tnitri
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tnitri,"E");cont++;}else {  strcpy(analisis[i].Tnitri,temp);	}


	// columna Vnitri
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tnitri,"E")==0){cont++;}else {  analisis[i].Vnitri=atof(temp);	}
	
 
 
    if ( strcmp(analisis[i].Tnitri,"E")!=0) {  
	   
	     // comprbar nitra +nitri  nitrato/50+nitri/3 <1
   
    if ((analisis[i].Vnitra/50+analisis[i].Vnitri/3)>1   &&  strcmp(analisis[i].Tnitra,"E")!=0 ){strcat(incumplimiento," *SUMA NITRITO+NITRATO ");strcat(incumplimiento,pBuffer);   }
	
	  if ( analisis[i].Vnitri>0.5) {PrintFloat( analisis[i].Vnitri,1,pBuffer);strcat(incumplimiento," *NITRITO ");strcat(incumplimiento,pBuffer);	}  }
	
	// columna Tcl2lib
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tcl2lib,"E");cont++;}else {  strcpy(analisis[i].Tcl2lib,temp);	}


	// columna Vcl2lib
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tcl2lib,"E")==0){cont++;}else {  analisis[i].Vcl2lib=atof(temp);
		
    if ( analisis[i].Vcl2lib>1.5) {PrintFloat( analisis[i].Vcl2lib,1,pBuffer);strcat(incumplimiento," *Cloro libre ");strcat(incumplimiento,pBuffer);	}
	}
   
   
   	// columna Tcl2cob
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tcl2cob,"E");cont++;}else {  strcpy(analisis[i].Tcl2cob,temp);	}


	// columna Vcl2cob
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tcl2cob,"E")==0){cont++;}else { 
	 analisis[i].Vcl2cob=atof(temp);	
   	 if ( analisis[i].Vcl2cob>2) {PrintFloat( analisis[i].Vcl2cob,1,pBuffer);strcat(miniincumplimiento," Cloro combinado  ");strcat(miniincumplimiento,pBuffer)	;	}
		}
	
	// columna Tentco
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tentco,"E");cont++;}else {  strcpy(analisis[i].Tentco,temp);	}


	// columna Ventco
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tentco,"E")==0){cont++;}else {  analisis[i].Ventco=atoi(temp);	}
    if ( strcmp(analisis[i].Tentco,"E")!=0 ) {  if ( analisis[i].Ventco>0) {PrintFloat( analisis[i].Ventco,0,pBuffer);strcat(incumplimiento," *ENTEROCOCOS ");strcat(incumplimiento,pBuffer);	} }
	
   	// columna Tbh22
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tbh22,"E");cont++;}else {  strcpy(analisis[i].Tbh22,temp);	}

	// columna Vbh22
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tbh22,"E")==0){cont++;}else {  analisis[i].Vbh22=atoi(temp);	}
    if ( strcmp(analisis[i].Tbh22,"E")!=0 ) {    if ( analisis[i].Vbh22>5000) {PrintFloat( analisis[i].Vbh22,0,pBuffer);strcat(miniincumplimiento," Aerobios ");strcat(miniincumplimiento,pBuffer)	;	}   }
	
   	// columna Tclper
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Tclper,"E");cont++;}else {  strcpy(analisis[i].Tclper,temp);	}


	// columna Vclper
	for (j=0;j<19;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Tclper,"E")==0){cont++;}else {  analisis[i].Vclper=atoi(temp);	}
    if ( strcmp(analisis[i].Tclper,"E")!=0 ) {if ( analisis[i].Vclper>0) {PrintFloat( analisis[i].Vclper,0,pBuffer);strcat(incumplimiento," *CLOSTRIDIOS ");strcat(incumplimiento,pBuffer);	}}
   
   	// columna Toxi
	for (j=0;j<19;j++){	 temp[j]='\0'; }  aux=cadena[cont];
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(analisis[i].Toxi,"E");cont++;}else {  strcpy(analisis[i].Toxi,temp);	}
    

	// columna Voxi
	for (j=0;j<3;j++){	 temp[j]='\0'; } aux=cadena[cont];
    for (j=0;j<18;j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(analisis[i].Toxi,"E")==0){;}else {  analisis[i].Voxi=atof(temp);	  }     
    if ( strcmp(analisis[i].Toxi,"E")!=0) {if ( analisis[i].Voxi>5) {PrintFloat( analisis[i].Voxi,1,pBuffer);strcat(miniincumplimiento," oxidabilidad ");strcat(miniincumplimiento,pBuffer);	}}


    //Evaluar si hay parametros minimos para calificar
	if(strcmp(analisis[i].Tconduc,"E")==0 || strcmp(analisis[i].Tamon,"E")==0 || strcmp(analisis[i].Tturb,"E")==0 || strcmp(analisis[i].Tcoto,"E")==0 || strcmp(analisis[i].Tecoli,"E")==0 ){
				
			fprintf(reporteEkuis,"** No hay suficientes parametros para evaluar esta muestra lims %d  ",analisis[i].idlims);
			printf("** No hay suficientes parametros para evaluar esta muestra lims %d  ",analisis[i].idlims);
			for (j=0;j<numanalisis;j++)
				{ if (reg[j].referencia==analisis[i].idlims){
				reg[j].calificacion=4;strcpy(reg[j].obs,"No se han analizado suficientes parametros para la calificacion. ");
				printf(" punto %d (%s)  fecha %s \n",reg[j].ekuis,reg[j].nombre,reg[j].fecharec);
				fprintf(reporteEkuis," punto %d (%s)  fecha %s \n",reg[j].ekuis,reg[j].nombre,reg[j].fecharec);
				}
		    }
	} // fin iterador general analisi basico

	// asigna incumplimientos
	
	numanalisisbasicos=i+1;
	
		if (strcmp(miniincumplimiento,"\0")!=0){
						for (j=0;j<numanalisis;j++)
				{ if (reg[j].referencia==analisis[i].idlims){strcpy(reg[j].miniincumplimientos,miniincumplimiento);printf("Miniincumplimiento %s lims num %d(%s)\n",miniincumplimiento,analisis[i].idlims,reg[j].nombre);}	}		
				}		
			
			
			
	if (strcmp(incumplimiento,"\0")!=0)
		{printf("Incumple %s lims num %d",incumplimiento,analisis[i].idlims);
		
			for (j=0;j<numanalisis;j++)
			 	
				if (strstr(incumplimiento,"*")!=NULL){  	for (j=0;j<numanalisis;j++)
				{ if (reg[j].referencia==analisis[i].idlims){
					printf("(%s)\n",reg[j].nombre);
				reg[j].calificacion=2; strcpy(reg[j].fraseincumplimientoBas,incumplimiento);}	}	
			   	}
    } // end del if
	
	
}//end for
   

	fclose(analisisekuis);
    printf("\nSe han procesado %d analisis de analisis basicos.\n",i);

    
    //metales
    
	printf("Procesando el fichero analisisMetales.csv\n ");			
	metales=fopen("EKUIS_analiticaMetales.csv","r");	
	if (metales==NULL){
		printf("\nError al abrir el fichero metales!!! No se ha encontrado\n");
		fprintf(reporteEkuis,"Error al abrir el fichero metales!!! No se ha encontrado\n");
		goto sinmetales;
	}
	
   	 
  	if((  cadena = (char*)realloc ( cadena,MAXmetales*sizeof(char) )   ) == NULL) {
   		printf("\nError al reasignar memoria.");
        return -1;
    };
    	
		  
    while( !feof(metales) ) {
  	 fgets(cadena,MAXmetales,metales);
  	 numanalisismetales++;	
    }
    
    numanalisismetales--;  //eliminar cabecera
    rewind(metales);
  
   	Analisismetales = (Metales*)malloc(numanalisismetales*sizeof(Metales));
    if (Analisismetales==NULL) {printf("\nError al asignar memoria a metales");
		}
   
//procesamiento cabecera de analitica metales


	fgets(cadena,MAXmetales,metales);
	   	
	token=strtok(cadena,",");
		
			if (token !=NULL){
	        cont=0;
			while (token !=NULL){
			switch (cont){
				case 0: 
				    if (strcmp(token,"ID Numeric")!=0) {printf( "Error en la lectura de cabecera id");exit(-1);}	
				    break;
				case 1: 
				if (strcmp(token,"AlTipo")!=0) {printf( "Error en la lectura de cabecera Al T");exit(-1);}	
				    break;
			    case 2:  
				     if (strcmp(token,"Al")!=0) {printf( "Error en la lectura de cabecera Al");exit(-1);}
				    break; 
			    case 3:
			    	 if (strcmp(token,"SbTipo")!=0) {printf( "Error en la lectura de cabecera Sb T");exit(-1);}
				    break;  
				case 4:
				    if (strcmp(token,"Sb")!=0) {printf( "Error en la lectura de cabecera Sb");exit(-1);}
				    break;  
				case 5:
					 if (strcmp(token,"AsTipo")!=0) {printf( "Error en la lectura de cabecera As T");exit(-1);}
				    break;  
				case 6:
				    if (strcmp(token,"As")!=0) {printf( "Error en la lectura de cabecera As");exit(-1);}
				    break;  
				case 7:
					 if (strcmp(token,"BaTipo")!=0) {printf( "Error en la lectura de cabecera Ba T");exit(-1);}
				    break;  
				case 8:
				     if (strcmp(token,"Ba")!=0) {printf( "Error en la lectura de cabecera Ba ");exit(-1);}
				    break;   
				case 9:
					  if (strcmp(token,"BTipo")!=0) {printf( "Error en la lectura de cabecera B T");exit(-1);}
				    break;  
				case 10:
				  if (strcmp(token,"B")!=0) {printf( "Error en la lectura de cabecera B");exit(-1);}
				    break;  
				case 11:
				 if (strcmp(token,"CdTipo")!=0) {printf( "Error en la lectura de cabecera Cd T");exit(-1);}
				    break;  
				case 12:
				    if (strcmp(token,"Cd")!=0) {printf( "Error en la lectura de cabecera Cd");exit(-1);}
				    break;  
				case 13:
					if (strcmp(token,"CuTipo")!=0) {printf( "Error en la lectura de cabecera Cu T");exit(-1);}
				    break;  
				case 14:
				      if (strcmp(token,"Cu")!=0) {printf( "Error en la lectura de cabecera Cu");exit(-1);}
				    break;  
				case 15:
					  if (strcmp(token,"CrTipo")!=0) {printf( "Error en la lectura de cabecera Cr T");exit(-1);}
				    break;  
				case 16:
				     if (strcmp(token,"Cr")!=0) {printf( "Error en la lectura de cabecera Cr");exit(-1);}
				    break;  
				case 17:
					 if (strcmp(token,"FeTipo")!=0) {printf( "Error en la lectura de cabecera Fe T");exit(-1);}
				    break; 
			   	case 18:
				      if (strcmp(token,"Fe")!=0) {printf( "Error en la lectura de cabecera Fe");exit(-1);}
				    break; 
				case 19:
					if (strcmp(token,"MnTipo")!=0) {printf( "Error en la lectura de cabecera Mn T");exit(-1);}
				    break; 
				case 20:
				    if (strcmp(token,"Mn")!=0) {printf( "Error en la lectura de cabecera Mn");exit(-1);}
				    break; 
				case 21:
					if (strcmp(token,"MoTipo")!=0) {printf( "Error en la lectura de cabecera Mo T");exit(-1);}
				    break; 
				case 22:
				  if (strcmp(token,"Mo")!=0) {printf( "Error en la lectura de cabecera Mo");exit(-1);}
				    break; 
				case 23:
					 if (strcmp(token,"NiTipo")!=0) {printf( "Error en la lectura de cabecera Ni T");exit(-1);}
				    break; 
				case 24:
				    if (strcmp(token,"Ni")!=0) {printf( "Error en la lectura de cabecera Ni");exit(-1);}
				    break; 
				case 25:
					if (strcmp(token,"PbTipo")!=0) {printf( "Error en la lectura de cabecera Pb T");exit(-1);}
				    break; 
				case 26:
				    if (strcmp(token,"Pb")!=0) {printf( "Error en la lectura de cabecera Pb");exit(-1);}
				    break; 
				case 27:
					 if (strcmp(token,"SeTipo")!=0) {printf( "Error en la lectura de cabecera Se T");exit(-1);}
				    break; 
				case 28:
				    if (strcmp(token,"Se")!=0) {printf( "Error en la lectura de cabecera Se");exit(-1);}
				    break; 
				case 29:
					 if (strcmp(token,"ZnTipo")!=0) {printf( "Error en la lectura de cabecera Zn T");exit(-1);}
				    break; 
				case 30:
				       if (token[0]!='Z'&&token[1]!='n') {printf( "Error en la lectura de cabecera Zn");exit(-1);}
				    break; 			    				    
	    }
			token=strtok(NULL,",");	
			cont++;
			} 		// end of while	
	} // end del if	
		
	printf("\nCabecera analisiMetales.csv correcto\n");
	
   
    
  // Procesamiento analisis metales
  
   

   
   // for(i=0;!feof(metales);i++){ // iterador general
      for(i=0;i<numanalisis;i++){ // iterador general            
       strcpy(incumplimiento,"\0");
       strcpy(miniincumplimiento,"\0");
	   fgets(cadena,MAXmetales,metales);
	    
         	for (j=0;j<19;j++){	 temp[j]='\0'; }	 
        cont=0;
		aux ='0';
	  
	  
	// columna idlims
	  
      for (j=0;aux!=',';j++){ aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

	if (strcmp(temp,"\0")==0){printf("\nError. Falta un idlims en linea num %d\n",i);exit(-1);}else {	Analisismetales[i].idlims=atol(temp);	}

	// columna Tal
     aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tal,"E");cont++;}else {  strcpy(Analisismetales[i].Tal,temp);	}

	// columna Val
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tal,"E")==0){cont++;}else {  Analisismetales[i].Val=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tal,"E")!=0 ) { if ( Analisismetales[i].Val>200) {PrintFloat( Analisismetales[i].Val,1,pBuffer);strcat(miniincumplimiento," Aluminio ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	// columna Tsb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tsb,"E");cont++;}else {  strcpy(Analisismetales[i].Tsb,temp);	}


	// columna Vsb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tsb,"E")==0){cont++;}else {  Analisismetales[i].Vsb=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tsb,"E")!=0 ) { if ( Analisismetales[i].Vsb>5) {PrintFloat( Analisismetales[i].Vsb,1,pBuffer);strcat(incumplimiento," *Sb ");strcat(incumplimiento,pBuffer);	}}  
    
   	// columna Tas
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tas,"E");cont++;}else {  strcpy(Analisismetales[i].Tas,temp);	}


	// columna Vas
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tas,"E")==0){cont++;}else {  Analisismetales[i].Vas=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tas,"E")!=0 ) { if ( Analisismetales[i].Vas>10) {PrintFloat( Analisismetales[i].Vsb,1,pBuffer);strcat(incumplimiento," *As ");strcat(incumplimiento,pBuffer);	}}  
    
   	// columna Tba
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tba,"E");cont++;}else {  strcpy(Analisismetales[i].Tba,temp);	}


	// columna Vba
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tba,"E")==0){cont++;}else {  Analisismetales[i].Vba=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tba,"E")!=0 ) { if ( Analisismetales[i].Vba>2000) {PrintFloat( Analisismetales[i].Vba,1,pBuffer);strcat(miniincumplimiento," Bario ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	// columna Tb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tb,"E");cont++;}else {  strcpy(Analisismetales[i].Tb,temp);	}


	// columna Vb  hay que convertir a mg/l desde microgramos
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tb,"E")==0){cont++;}else {  Analisismetales[i].Vb=atof(temp)/1000;	}
    if ( strcmp(Analisismetales[i].Tb,"E")!=0 ) { if ( Analisismetales[i].Vb>1) {PrintFloat( Analisismetales[i].Vb,1,pBuffer);strcat(incumplimiento," *Boro ");strcat(incumplimiento,pBuffer);	}}  
    
    
   	// columna Tcd
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tcd,"E");cont++;}else {  strcpy(Analisismetales[i].Tcd,temp);	}


	// columna Vcd
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tcd,"E")==0){cont++;}else {  Analisismetales[i].Vcd=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tcd,"E")!=0 ) { if ( Analisismetales[i].Vcd>5) {PrintFloat( Analisismetales[i].Vcd,1,pBuffer);strcat(incumplimiento," *Cd ");strcat(incumplimiento,pBuffer);	}}  
    
   	// columna Tcu
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tcu,"E");cont++;}else {  strcpy(Analisismetales[i].Tcu,temp);	}


	// columna Vcu  hay que convertir en mg/l
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tcu,"E")==0){cont++;}else { Analisismetales[i].Vcu=atof(temp)/1000; 	}
    if ( strcmp(Analisismetales[i].Tcu,"E")!=0 ) { if ( Analisismetales[i].Vcu>2) {PrintFloat( Analisismetales[i].Vcu,1,pBuffer);strcat(incumplimiento," *Cu ");strcat(incumplimiento,pBuffer);	}}  
    
    
    // columna Tcr
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tcr,"E");cont++;}else {  strcpy(Analisismetales[i].Tcr,temp);	}


	// columna Vcr
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tcr,"E")==0){cont++;}else {  Analisismetales[i].Vcr=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tcr,"E")!=0 ) { if ( Analisismetales[i].Vcr>50) {PrintFloat( Analisismetales[i].Vcr,1,pBuffer);strcat(incumplimiento," *Cr ");strcat(incumplimiento,pBuffer);	}}  
    
    
    // columna Tfe
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tfe,"E");cont++;}else {  strcpy(Analisismetales[i].Tfe,temp);	}


	// columna Vfe
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tfe,"E")==0){cont++;}else {  Analisismetales[i].Vfe=atof(temp);	}
   
    if ( strcmp(Analisismetales[i].Tfe,"E")!=0) { if ( Analisismetales[i].Vfe>200) {PrintFloat( Analisismetales[i].Vfe,1,pBuffer);strcat(miniincumplimiento," Fe ");strcat(miniincumplimiento,pBuffer);	}}  
    
    // columna Tmn
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tmn,"E");cont++;}else {  strcpy(Analisismetales[i].Tmn,temp);	}


	// columna Vmn
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tmn,"E")==0){cont++;}else {  Analisismetales[i].Vmn=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tmn,"E")!=0 ) { if ( Analisismetales[i].Vmn>50) {PrintFloat( Analisismetales[i].Vmn,1,pBuffer);strcat(miniincumplimiento," Mn ");strcat(miniincumplimiento,pBuffer);	}}  
    
    // columna Tmo
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tmo,"E");cont++;}else {  strcpy(Analisismetales[i].Tmo,temp);	}


	// columna Vmo
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tmo,"E")==0){cont++;}else {  Analisismetales[i].Vmo=atof(temp);	}
    if (strcmp(Analisismetales[i].Tmo,"E")!=0 ) { if ( Analisismetales[i].Vmo>70) {PrintFloat( Analisismetales[i].Vmo,1,pBuffer);strcat(miniincumplimiento," Molibdeno ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   // columna Tni
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tni,"E");cont++;}else {  strcpy(Analisismetales[i].Tni,temp);	}


	// columna Vni
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tni,"E")==0){cont++;}else {  Analisismetales[i].Vni=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tni,"E")!=0 ) { if ( Analisismetales[i].Vni>20) {PrintFloat( Analisismetales[i].Vni,1,pBuffer);strcat(incumplimiento," *Ni ");strcat(incumplimiento,pBuffer);	}}  
    
   	// columna Tpb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tpb,"E");cont++;}else {  strcpy(Analisismetales[i].Tpb,temp);	}


	// columna Vpb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tpb,"E")==0){cont++;}else { Analisismetales[i].Vpb=atof(temp); 	}
    if (strcmp(Analisismetales[i].Tpb,"E")!=0 ) { if ( Analisismetales[i].Vpb>10) {PrintFloat( Analisismetales[i].Vpb,1,pBuffer);strcat(incumplimiento," *Pb ");strcat(incumplimiento,pBuffer);	}}  
    
   
    // columna Tse
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tse,"E");cont++;}else {  strcpy(Analisismetales[i].Tse,temp);	}


	// columna Vse
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tse,"E")==0){cont++;}else {  Analisismetales[i].Vse=atof(temp);	}
    if ( strcmp(Analisismetales[i].Tse,"E")!=0 ) { if ( Analisismetales[i].Vse>10) {PrintFloat( Analisismetales[i].Vse,1,pBuffer);strcat(incumplimiento," *Se ");strcat(incumplimiento,pBuffer);	}}  
    
    
    // columna Tzn
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(Analisismetales[i].Tzn,"E");cont++;}else {  strcpy(Analisismetales[i].Tzn,temp);	}


	// columna Vzn
    aux=cadena[cont];
	for (j=0;j<5;j++){	 temp[j]='\0'; } 	
    for (j=0;j<8;j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(Analisismetales[i].Tzn,"E")==0){;}else {  Analisismetales[i].Vzn=atof(temp);  	}
    if (  strcmp(Analisismetales[i].Tzn,"E")!=0 ) { if ( Analisismetales[i].Vzn>5000) {PrintFloat( Analisismetales[i].Vzn,1,pBuffer);strcat(miniincumplimiento," Zinc. ");strcat(miniincumplimiento,pBuffer)	;	}}  
    

    
    // Asignacion incumplimientos metales
    
	if (strcmp(incumplimiento,"\0")!=0)
		{printf("Incumple %s lims num %d",incumplimiento,Analisismetales[i].idlims);for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==Analisismetales[i].idlims){
		printf("(%s)\n",reg[j].nombre);
		reg[j].calificacion=2;  strcpy(reg[j].fraseincumplimientoMet,incumplimiento);}
		}		
		} 
	

	if (strcmp(miniincumplimiento,"\0")!=0){
				for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==Analisismetales[i].idlims){printf("Miniincumplimiento %s lims num %d(%s)\n",miniincumplimiento,Analisismetales[i].idlims,reg[j].nombre);strcpy(reg[j].miniincumplimientos,miniincumplimiento);}	}		
	}
	 	
		
	
}//end for general metales
   	

	fclose(metales);
    printf("\nSe han procesado %d analisis de metales.\n",numanalisismetales);
    
    
  sinmetales:  
   
    // Inicializacion VOCS1
       
 
	printf("Procesando el fichero analisisVOC1.csv\n ");			
	voc1=fopen("EKUIS_voc1.csv","r");	
	if (voc1==NULL){
		printf("\nError al abrir el fichero VOC1!!! Quizas no existe\n");
		fprintf(reporteEkuis,"\nError al abrir el fichero VOC1!!! Quizas no existe\n");
		goto sinvoc;
	}
	
   	 
  	if((  cadena = (char*)realloc ( cadena,MAXVOC1*sizeof(char) )   ) == NULL) {
   		printf("\nError al reasignar memoria VOC 1.");
        return -1;
    };
    	
		  
    while( !feof(voc1) ) {
  	 fgets(cadena,MAXVOC1,voc1);
  	 numanalisisvoc1++;	
   }
   
   numanalisisvoc1--; // descontar cabecera
   rewind(voc1);
   
   voc = (VOCS*)malloc(numanalisisvoc1*sizeof(VOCS));
   
   	if (voc==NULL) {printf("\nError al asignar memoria a VOCS");
		}
   
    
   //Lectura de VOC1
    
  	   fgets(cadena,MAXVOC1,voc1); 
      
    // comprobar cabecera de VOC 1
     
    if (strcmp(cadena,"ID Numeric,C4C2t,C4C2,TCEt,TCE,DCEt,DCE,DCPt,DCP,DCB13t,DCB13,DCB14t,DCB14,BENt,BEN,BDCMt,BDCM,BR3CHt,BR3CH,C13DCPt,C13DCP,CBt,CB,CL3CHt,CL3CH,CLVIt,CLVI\n")!=0){
    	printf("ERROR gordo a la hora de leer cabecera VOC1, no corresponden:\n -----> Debe ser: ID Numeric,C4C2t,C4C2,TCEt,TCE,DCEt,DCE,DCPt,DCP,DCB13t,DCB13,DCB14t,DCB14,BENt,BEN,BDCMt,BDCM,BR3CHt,BR3CH,C13DCPt,C13DCP,CBt,CB,CL3CHt,CL3CH,CLVIt,CLVI \n  sin embargo es: %s\n",cadena);
	} else {printf("Lectura cabecera Voc1 correcta, hay %d analisis de voc1.\n",numanalisisvoc1);	}
  
    for(i=0;!feof(voc1);i++){ // iterador general
             
       strcpy(incumplimiento,"\0");
       strcpy(miniincumplimiento,"\0");
	   fgets(cadena,MAXVOC1,voc1); 
	
       for (j=0;j<19;j++){	 temp[j]='\0'; }	 
       cont=0;
       aux ='0';
	 
	  // columna idlims
	  
      for (j=0;aux!=',';j++){ aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

	if (strcmp(temp,"\0")==0){printf("\nError leyendo vocs1. Falta un idlims en linea num %d\n",i);exit(-1);}else {	voc[i].idlims=atol(temp);	}


   	//1 columna Tc4c2 1122tetracloroeteno
     aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tc4c2,"E");cont++;}else {  strcpy(voc[i].Tc4c2,temp);	}

	// columna Vc4c2
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tc4c2,"E")==0){cont++;}else {  voc[i].Vc4c2=atof(temp);	}
    if ( strcmp(voc[i].Tc4c2,"E")!=0 ) 
	{ if ( voc[i].Vc4c2>200) 	
    {PrintFloat( voc[i].Vc4c2,1,pBuffer);strcat(miniincumplimiento," tetracloroeteno ");strcat(miniincumplimiento,pBuffer)	;	}} 
    
   	//2 columna T112tce  112tricloroetano
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].T112tce,"E");cont++;}else {  strcpy(voc[i].T112tce,temp);	}

	// columna V112tce
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].T112tce,"E")==0){cont++;}else {  voc[i].V112tce=atof(temp);	}
    if ( strcmp(voc[i].T112tce,"E")!=0) { if ( voc[i].V112tce>200) {PrintFloat( voc[i].V112tce,0,pBuffer);strcat(miniincumplimiento, " Tricloroetano "); strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	//3 columna T12dce   12dicloroetano
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].T12dce,"E");cont++;}else {  strcpy(voc[i].T12dce,temp);	}

	// columna V12dce
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].T12dce,"E")==0){cont++;}else {  voc[i].V12dce=atof(temp);	}
    if (strcmp(voc[i].T12dce,"E")!=0 ) { if ( voc[i].V12dce>3) {PrintFloat( voc[i].V12dce,1,pBuffer);strcat(incumplimiento," *Dicloroetano ");strcat(incumplimiento,pBuffer);	}}  
    
    
   	//4 columna T12dcp  12 dicloropropano
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].T12dcp,"E");cont++;}else {  strcpy(voc[i].T12dcp,temp);	}

	// columna V12dcp
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].T12dcp,"E")==0){cont++;}else {  voc[i].V12dcp=atof(temp);	}
    if (strcmp(voc[i].T12dcp,"E")!=0 ) { if ( voc[i].V12dcp>200) {PrintFloat( voc[i].V12dcp,0,pBuffer);strcat(miniincumplimiento," Dicloropropano ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	//5 columna T13dcb 13 diclorobenceno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].T13dcb,"E");cont++;}else {  strcpy(voc[i].T13dcb,temp);	}

	// columna V13dcb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].T13dcb,"E")==0){cont++;}else {  voc[i].V13dcb=atof(temp);	}
    if ( strcmp(voc[i].T13dcb,"E")!=0 ) { if ( voc[i].V13dcb>200) {PrintFloat( voc[i].V13dcb,0,pBuffer);strcat(miniincumplimiento," 13diclorobenceno ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	//6 columna T14dcb  14 diclorobenceno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].T14dcb,"E");cont++;}else {  strcpy(voc[i].T14dcb,temp);	}

	// columna V14dcb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].T14dcb,"E")==0){cont++;}else {  voc[i].V14dcb=atof(temp);	}
    if ( strcmp(voc[i].T14dcb,"E")!=0) { if ( voc[i].V14dcb>200) {PrintFloat( voc[i].V14dcb,0,pBuffer);strcat(miniincumplimiento," 14diclorobenceno ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
    
    //7 columna Tben  benceno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tben,"E");cont++;}else {  strcpy(voc[i].Tben,temp);	}

	// columna Vben
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tben,"E")==0){cont++;}else {  voc[i].Vben=atof(temp);         	}
    if (strcmp(voc[i].Tben,"E")!=0) { if ( voc[i].Vben>1) {PrintFloat( voc[i].Vben,0,pBuffer);strcat(incumplimiento," *BENCENO ");strcat(incumplimiento,pBuffer)	;	}}  
    
    //8 columna Tbdcm  bromodiclorometano
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tbdcm,"E");cont++;}else {  strcpy(voc[i].Tbdcm,temp);	}

	// columna Vbdcm
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tbdcm,"E")==0){cont++;}else {  voc[i].Vbdcm=atof(temp);	}
    if ( strcmp(voc[i].Tbdcm,"E")!=0) 
	{ if ( voc[i].Vbdcm>100)  
     {PrintFloat( voc[i].Vbdcm,1,pBuffer);strcat(incumplimiento," Bromodiclorometano ");strcat(incumplimiento,pBuffer);	}} 
     
  	//9 columna Tbr3ch  bromoformo
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tbr3ch,"E");cont++;}else {  strcpy(voc[i].Tbr3ch,temp);	}

	// columna Vbr3ch   
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tbr3ch,"E")==0){cont++;}else {  voc[i].Vbr3ch=atof(temp);	}
    if ( strcmp(voc[i].Tbr3ch,"E")!=0 ) { if ( voc[i].Vbr3ch>100) {PrintFloat( voc[i].Vbr3ch,0,pBuffer);strcat(miniincumplimiento," bromoformo ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
    //10 columna Tc13dcp  cis13dicloropropeno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tc13dcp,"E");cont++;}else {  strcpy(voc[i].Tc13dcp,temp);	}

	// columna Vc13dcp
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tc13dcp,"E")==0){cont++;}else {  voc[i].Vc13dcp=atof(temp);	}
    if ( strcmp(voc[i].Tc13dcp,"E")!=0 ) { if ( voc[i].Vc13dcp>200) {PrintFloat( voc[i].Vc13dcp,0,pBuffer);strcat(miniincumplimiento," dicloropropeno ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
    //11 columna Tcb   clorobenceno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tcb,"E");cont++;}else {  strcpy(voc[i].Tcb,temp);	}

	// columna Vcb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tcb,"E")==0){cont++;}else {  voc[i].Vcb=atof(temp);	}
    if ( strcmp(voc[i].Tcb,"E")!=0 ) { if ( voc[i].Vcb>200) {PrintFloat( voc[i].Vcb,0,pBuffer);strcat(miniincumplimiento," clorobenceno ");strcat(miniincumplimiento,pBuffer)	;	}}  
    
   	//12 columna Tcl3ch  cloroformo
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tcl3ch,"E");cont++;}else {  strcpy(voc[i].Tcl3ch,temp);	}

	// columna Vcl3ch
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tcl3ch,"E")==0){cont++;}else {  voc[i].Vcl3ch=atof(temp);	}
    if ( strcmp(voc[i].Tcl3ch,"E")!=0) { if ( voc[i].Vcl3ch>100) 
      {PrintFloat( voc[i].Vcl3ch,2,pBuffer);strcat(incumplimiento," CL3CH ");strcat(miniincumplimiento," cloroformo ");strcat(miniincumplimiento,pBuffer)	;	}} 
   
   	//13 columna Tclvi  cloruro vinilo
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tclvi,"E");cont++;}else {  strcpy(voc[i].Tclvi,temp);	}

	// columna Vclvi
    aux=cadena[cont];
	for (j=0;j<5;j++){	 temp[j]='\0'; } 	
    for (j=0;j<8;j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tclvi,"E")==0){cont++;}else {  voc[i].Vclvi=atof(temp);	}
    if ( strcmp(voc[i].Tclvi,"E")!=0 ) { if ( voc[i].Vclvi>0.5) {PrintFloat( voc[i].Vclvi,0,pBuffer);strcat(miniincumplimiento," cloruro vinilo ");strcat(miniincumplimiento,pBuffer)	;	}}  
    

  // Asignacion incumplimientos VOC1
	if (strcmp(incumplimiento,"\0")!=0)
		{printf("Incumple %s lims num %d\n",incumplimiento,voc[i].idlims);for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==voc[i].idlims){ reg[j].calificacion=2;strcpy(reg[j].fraseincumplimientoVoc1,incumplimiento);   }
		}
		} 
		
    if (strcmp(miniincumplimiento,"\0")!=0){
				for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==voc[i].idlims){strcpy(reg[j].miniincumplimientos,miniincumplimiento);printf("Miniincumplimiento %s lims num %d\n",miniincumplimiento,voc[i].idlims);}	}		
	}
	   
    
} // end del iterador general for voc1

    printf("\nSe han procesado %d analisis de analisis voc1.\n",numanalisisvoc1);
    close(voc1);
    

    // Inicializacion VOCS2
    
     
    voc2=fopen("EKUIS_voc2.csv","r");
	printf("\nProcesando el fichero analisisVOC2.csv\n ");			
		
	if (voc2==NULL){
		printf("\nError al abrir el fichero VOC2");
	}
	
   	 
  	if((  cadena = (char*)realloc ( cadena,MAXVOC2*sizeof(char) )   ) == NULL) {
   		printf("\nError al reasignar memoria VOC 2.");
        return -1;
    };
    	
	// numanalisisvoc1 y voc 2 deberian ser lo mismo	  
    while( !feof(voc2) ) {
  	 fgets(cadena,MAXVOC2,voc2);
  	 numanalisisvoc2++;	
    }
    
    numanalisisvoc2--;    // descontar cabecera
    rewind(voc2);
   
    fgets(cadena,MAXVOC1,voc2); 
    
    
          
    // comprobar cabecera de VOC 2
     
    if (strcmp(cadena,"ID Numeric,ALDt,ALD,DIELt,DIEL,HPCLt,HPCL,HPCLEt,HPCLE,PLTOTt,PLTOT,THMt,THM,TOLt,TOL,ETBt,ETB,CLETt,CLET\n")!=0){
    	printf("ERROR gordo a la hora de leer cabecera VOC2, no corresponden:\n -----> Debe ser: ID Numeric,ALDt,ALD,DIELt,DIEL,HPCLt,HPCL,HPCLEt,HPCLE,PLTOTt,PLTOT,THMt,THM,TOLt,TOL,ETBt,ETB,CLETt,CLET \n  sin embargo es: %s\n",cadena);
	} else {printf("Lectura cabecera Voc1 correcta.\n");	}
  
       
   	if((  voc = (VOCS*)realloc ( voc,numanalisisvoc2*sizeof(VOCS) )   ) == NULL) {
   		printf("\nError al reasignar memoria.");
        return -1;
    };
    
    
   //Lectura de VOC2
    
   // fgets(cadena,MAXVOC2,voc2); 
  
    for(i=0;!feof(voc2);i++){ // iterador general
             
	   strcpy(incumplimiento,"\0");
	   strcpy(miniincumplimiento,"\0");
	   fgets(cadena,MAXVOC2,voc2); 
	   for (j=0;j<19;j++){	 temp[j]='\0'; }	 
	   cont=0;
	   aux ='0';
		 
	  
   // columna idlims
	  
    for (j=0;aux!=',';j++){ aux=cadena[cont];  cont++;
	    if (aux!=','){temp[j]=aux;  }
       }

	if (strcmp(temp,"\0")==0){printf("\nError leyendo vocs2. Falta un idlims en linea num %d\n",i);exit(-1);}else {	voc[i].idlims=atol(temp);	}


	//1 columna Taldr  aldrin
	
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tald,"E");cont++;}else {  strcpy(voc[i].Tald,temp);	}

	// columna Valdr
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tald,"E")==0){cont++;}else {  voc[i].Vald=atof(temp);	}
    if (strcmp(voc[i].Tald,"E")!=0 ) { if ( voc[i].Vald>30) {PrintFloat( voc[i].Vald,0,pBuffer);strcat(incumplimiento," *ALDRIN ");strcat(incumplimiento,pBuffer)	;	}}  
    
    //2 columna Tdiel   dieldrin
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tdiel,"E");cont++;}else {  strcpy(voc[i].Tdiel,temp);	}

	// columna Vdiel
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tdiel,"E")==0){cont++;}else {  voc[i].Vdiel=atof(temp);	}
    if ( strcmp(voc[i].Tdiel,"E")!=0) { if ( voc[i].Vdiel>30) {PrintFloat( voc[i].Vdiel,0,pBuffer);strcat(incumplimiento," *DIELDRIN ");strcat(incumplimiento,pBuffer)	;	}}
    
	//3 columna Thpcl   heptacloro
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Thpcl,"E");cont++;}else {  strcpy(voc[i].Thpcl,temp);	}

	// columna Vhpcl
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Thpcl,"E")==0){cont++;}else {  voc[i].Vhpcl=atof(temp);          	}
    if ( strcmp(voc[i].Thpcl,"E")!=0 ) { if ( voc[i].Vhpcl>30) {PrintFloat( voc[i].Vhpcl,0,pBuffer);strcat(incumplimiento," *HEPTACLORO ");strcat(incumplimiento,pBuffer)	;	}}  
    
  	//3 columna Thpcle  heptacloroepoxi
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Thpcle,"E");cont++;}else {  strcpy(voc[i].Thpcle,temp);	}

	// columna Vhpcle
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Thpcle,"E")==0){cont++;}else {  voc[i].Vhpcle=atof(temp);	}
    if ( strcmp(voc[i].Thpcle,"E")!=0 ) { if ( voc[i].Vhpcle>30) {PrintFloat( voc[i].Vhpcle,0,pBuffer);strcat(incumplimiento," *Heptacloro epoxido ");strcat(incumplimiento,pBuffer)	;	}}  
    
   	//4 columna Tpltot  plaguicida total
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tpltot,"E");cont++;}else {  strcpy(voc[i].Tpltot,temp);	}

	// columna Vpltot   cambio de unidades de nanogramos a microgramos
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tpltot,"E")==0){cont++;}else {  voc[i].Vpltot=atof(temp)/1000;	}
    if ( strcmp(voc[i].Tpltot,"E")!=0 ) { if ( voc[i].Vpltot>0.5) {PrintFloat( voc[i].Vpltot,0,pBuffer);strcat(incumplimiento," *Plaguicidas totales  ");strcat(incumplimiento,pBuffer)	;	}}  
    
    //5 columna Tthm   suma trihalometanos
     aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tthm,"E");cont++;}else {  strcpy(voc[i].Tthm,temp);	}

	// columna Vthm
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tthm,"E")==0){cont++;}else {  voc[i].Vthm=atof(temp);	}
    if ( strcmp(voc[i].Tthm,"E")!=0 ) { if ( voc[i].Vthm>100) {PrintFloat( voc[i].Vthm,1,pBuffer);strcat(incumplimiento," *Suma trihalometanos ");strcat(incumplimiento,pBuffer);	}} 
  
  	//6 columna Ttol   tolueno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Ttol,"E");cont++;}else {  strcpy(voc[i].Ttol,temp);	}

	// columna Vtol
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Ttol,"E")==0){cont++;}else {  voc[i].Vtol=atof(temp);	}
    if ( strcmp(voc[i].Ttol,"E")!=0) { if ( voc[i].Vtol>200) {PrintFloat( voc[i].Vtol,0,pBuffer);strcat(miniincumplimiento," tolueno ");strcat(miniincumplimiento,pBuffer)	;	}}  
        
   	//7 columna Tetb    etilbenceno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tetb,"E");cont++;}else {  strcpy(voc[i].Tetb,temp);	}

	// columna Vetb
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; } 	
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tetb,"E")==0){cont++;}else {  voc[i].Vetb=atof(temp);	}
    if ( strcmp(voc[i].Tetb,"E")!=0 ) { if ( voc[i].Vetb>200) {PrintFloat( voc[i].Vetb,0,pBuffer);strcat(miniincumplimiento," etilbenceno ");strcat(miniincumplimiento,pBuffer)	;	}}  
       
   	//8 columna TCLET   tricloroeteno mas tetracloroeteno
    aux=cadena[cont];
	for (j=0;j<19;j++){	 temp[j]='\0'; }  
    for (j=0;aux!=',';j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0){strcpy(voc[i].Tclet,"E");cont++;}else {  strcpy(voc[i].Tclet,temp);	}

	// columna Vclet
    aux=cadena[cont];
	for (j=0;j<5;j++){	 temp[j]='\0'; } 	
    for (j=0;j<8;j++){ aux=cadena[cont]; cont++;	 if (aux!=','){temp[j]=aux;  }   }
	if (strcmp(temp,"\0")==0 || strcmp(voc[i].Tclet,"E")==0){cont++;}else {  voc[i].Vclet=atof(temp);	}
    if (  strcmp(voc[i].Tclet,"E")!=0) { if ( voc[i].Vclet>10) {PrintFloat( voc[i].Vclet,0,pBuffer);strcat(incumplimiento," *Tricloroeteno+tetracloroeteno ");	strcat(incumplimiento,pBuffer)	;}}  
    
     // asigna incumplimientos VOC1
  
	if (strcmp(incumplimiento,"\0")!=0)
		{printf("Incumple %s lims num %d",incumplimiento,voc[i].idlims);for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==voc[i].idlims){ 
		printf("(%s)\n",reg[j].nombre);
		reg[j].calificacion=2;strcpy(reg[j].fraseincumplimientoVoc2,incumplimiento);}
		}	
	} 
	
  	if (strcmp(miniincumplimiento,"\0")!=0){
				for (j=0;j<numanalisis;j++)
		{ if (reg[j].referencia==voc[i].idlims){strcpy(reg[j].miniincumplimientos,miniincumplimiento);printf("Miniincumplimiento %s lims num %d(%s)\n",miniincumplimiento,voc[i].idlims,reg[j].nombre);}	}		
	}
	 	
     
  
}
//end for iterador general vocs2    

    
    
   printf("\nSe han procesado %d analisis de analisis voc2.\n",numanalisisvoc2);
 
  
    
   close(voc2);
   sinvoc:
   free(cadena);
    
for (i=0;i<numanalisis;i++){   //conteo incumplimientos voc2
	
		if (reg[i].calificacion==1){ apta++;
		}else if (reg[i].calificacion==2){noapta++;
		}else if (reg[i].calificacion==3){sincalif++;
	    }else if (reg[i].calificacion==4){cumple++;	}
		
		
		//asigna frases de incumplimiento
		
	  
   			strcpy(reg[i].obscali,"\0");
   			if(strcmp(reg[i].fraseincumplimientoBas,"\0")!=0 || strcmp(reg[i].fraseincumplimientoBas,"\0")!=0 || strcmp(reg[i].fraseincumplimientoMet,"\0")!=0  || strcmp(reg[i].fraseincumplimientoVoc1,"\0")!=0 ||
			   strcmp(reg[i].fraseincumplimientoVoc2,"\0")!=0 || strcmp(reg[i].miniincumplimientos,"\0")!=0){	strcat(reg[i].obscali,"Incumple: ");
			 }
   		
        	strcat(reg[i].obscali,reg[i].fraseincumplimientoBas);
        	strcat(reg[i].obscali,reg[i].fraseincumplimientoMet);
        	strcat(reg[i].obscali,reg[i].fraseincumplimientoVoc1);
        	strcat(reg[i].obscali,reg[i].fraseincumplimientoVoc2);
        	strcat(reg[i].obscali,reg[i].miniincumplimientos);						
}

 printf("\nResultado calificacion de potabilidad para los %d analisis:  APTAS: %d   NO APTAS: %d SIN CALIF: %d CUMPLE:%d\n",numanalisis,apta,noapta,sincalif,cumple);


// impresion reporte en pantalla


 printf("Fecha auth min: %s  fecha auth max: %s\n",reg[min].fechaaut,reg[max].fechaaut);


	printf("\nSe han generado las siguientes muestras: ");
	for (i=0;i<numanalisis;i++){
	  printf(" %d ,",reg[i].referencia);
	}
	printf("\n");
	printf("Resumen: Grifo: %d  Supervision: %d  completos: %d  libres: %d  Total analisis: %d\n",contgrifo,contsuperv,contcompleto,contlibre,numanalisis );
	printf(" Fecha inicial: %s   Fecha final:%s \n",reg[0].fecharec,reg[i-1].fecharec );
	
		
	time_t tiempo = time(0);   //calculo fecha actual
    struct tm *tlocal = localtime(&tiempo);
    strftime(hoy,128,"%d/%m/%y %H:%M:%S",tlocal);
    printf("Fecha actual: %s\n",hoy);

jump:
printf("\n-------------------------------------------------------------------------------------------------------------------------------------------------------------------\n");
printf("\nMENU pulse opcion:  1---Generar EKUIS  2---Cambiar calificacion  3---Redactar Observaciones calificacion  4---Redactar Observaciones laboratorio  5---Cambiar motivo\n");
scanf("%d",&opcion);
flag=0;
if (opcion==1){goto empezar;}

if (opcion==2){
	
	printf("Introduzca num Lims ");
	scanf("%d",&lims);
	printf("\nIntroduzca la calificacion (1-Apta para consumo 2-No apta para consumo 3-Sin calificacion  4-Cumple valores parametricos) ");
	scanf("%d",&cali);
	
	if (cali<0 || cali>4) {
	printf("\nEsa calificacion es incorrecta!\n"); goto jump;}
	for (i=0;i<numanalisis;i++){
		if (reg[i].referencia==lims){reg[i].calificacion=cali;
		flag=1;
		printf("La calificacion de %d es ahora %d\n",reg[i].referencia,reg[i].calificacion);
		fprintf(reporteEkuis,"Modificacion usuario: La calificacion de %d es %d\n",reg[i].referencia,reg[i].calificacion);
		}
	}
	if (flag==0){printf("No se ha podido realizar la operacion, quizas el id LIMS no existe.\n");
	}	
}



if (opcion==3){
	
	printf("Introduzca num Lims ");
	scanf("%d",&lims);
	printf("\nIntroduzca observaciones a la calificacion (max 100 caracteres) ");
	fflush(stdin);
	scanf("%[^\n]s",texto);
		if (strlen(texto)>100){printf("\nERROR, hay que escribir menos de 100 caracteres.\n"); goto jump;
	}
	
	for (i=0;i<numanalisis;i++){
		if (reg[i].referencia==lims){
			flag=1;
		strcpy(reg[i].obscali,texto);
		printf("\nModificacion usuario: La observacion cali de %d es %s\n",reg[i].referencia,reg[i].obscali);
			fprintf(reporteEkuis,"\nModificacion usuario: La observacion cali de %d es %s\n",reg[i].referencia,reg[i].obscali);
		}	
}
if (flag==0){printf("No se ha podido realizar la operacion, quizas el id LIMS no existe.\n");}
}

if (opcion==4){
	
	printf("\nIntroduzca num Lims ");
	scanf("%d",&lims);
	printf("\nIntroduzca observaciones generales (max 100 caracteres) ");
	fflush(stdin);
	scanf("%[^\n]s",texto);
	if (strlen(texto)>100){printf("\nERROR, hay que escribir menos de 100 caracteres."); goto jump;	}

	for (i=0;i<numanalisis;i++){
		if (reg[i].referencia==lims){
			flag=1;
		strcpy(reg[i].obs,texto);
			printf("\nModificacion usuario: La observacion de %d es %s\n",reg[i].referencia,reg[i].obs);
			fprintf(reporteEkuis,"\nModificacion usuario: La observacion de %d es %s\n",reg[i].referencia,reg[i].obs);
		}
	}
	if (flag==0){printf("No se ha podido realizar la operacion, quizas el id LIMS no existe.\n");}
}


if (opcion==5){
	
	printf("Introduzca num Lims ");
	scanf("%d",&lims);
	printf("\nIntroduzca Motivo (1-Programacion  2-Incidencia  3-Vigilancia sanitaria  4-Comprobar incumplimiento) \n");
	scanf("%d",&cali);
		if (cali<0 || cali>4) {
		printf("\nEse codigo de motivo es incorrecto!"); goto jump;}
	for (i=0;i<numanalisis;i++){
		flag=1;
		if (reg[i].referencia==lims){reg[i].motivo=cali;
			printf("\nEl motivo de %d es ahora %d\n",reg[i].referencia,reg[i].motivo);
			fprintf(reporteEkuis,"\nModificacion usuario: El motivo de %d es %d\n",reg[i].referencia,reg[i].motivo);
		}
	}
if (flag==0){printf("No se ha podido realizar la operacion, quizas el id LIMS no existe.\n");}

}


goto jump;

empezar:

	
// reporte 
  
 
     fprintf(reporteEkuis,"\n*** Fecha actual: %s \n",hoy);
     fprintf(reporteEkuis,"*** Fecha auth min: %s  fecha auth max: %s\n",reg[min].fechaaut,reg[max].fechaaut);
     fprintf(reporteEkuis,"*** Id lims desde %d hasta %d\n",reg[0].referencia,reg[numanalisis-1].referencia);
  
     fprintf(reporteEkuis,"*** Grifo:%d Supervision: %d completos: %d libres: %d total muestras: %d \n",contgrifo,contsuperv,contcompleto,contlibre,numanalisis);
    
	 fprintf(reporteEkuis,"*** Fecha recogida menor:%s  fecha mayor:%s\n",reg[0].fecharec, reg[numanalisis-1].fecharec);
	 fprintf(reporteEkuis,"*** analisis basicos realizados: %d  Test metales realizados:%d  Test voc1 realizados:%d    Test voc2 realizados:%d\n",numanalisisbasicos,numanalisismetales,numanalisisvoc1,numanalisisvoc2); 
	 fprintf(reporteEkuis,"\n*** Resultado calificacion de potabilidad para los %d muestras:  APTAS: %d   NO APTAS: %d SIN CALIF. %d  CUMPLE %d\n\n",numanalisis,apta,noapta,sincalif,cumple);
	 
	 for (j=0;j<numanalisis;j++){
	 
	 	if (strlen(reg[j].obscali)>100){
	 		printf("ERROR: El campo observaciones calificacion de %d es mayor a 100 caracteres(%d), hay que recortar;.\n%s\n",reg[j].referencia,strlen(reg[j].obscali),reg[j].obscali);
	 		fprintf(reporteEkuis,"ERROR: El campo observaciones calificacion de %d es mayor a 100 caracteres, hay que recortar:;.\n%s\n",reg[j].referencia,strlen(reg[j].obscali),reg[j].obscali);
		 }
		 
		if (strlen(reg[j].obs)>100){
	 		printf("ERROR: El campo observaciones de laboratorio de %d es mayor a 100 caracteres(%d), hay que recortar:\n%s\n",reg[j].referencia,strlen(reg[j].obs),reg[j].obs);
	 		fprintf(reporteEkuis,"ERROR: El campo observaciones laboratorio de %d es mayor a 100 caracteres (%d)), hay que recortar:;.\n%s\n",reg[j].referencia,strlen(reg[j].obs),reg[j].obs);
		 }	  	 	

	 	if (reg[j].calificacion==2){fprintf(reporteEkuis,"*punto %d (%s) No potable lims %d por los test: %s %s %s %s %s\n",reg[j].ekuis,reg[j].nombre,reg[j].referencia,reg[j].fraseincumplimientoMet,reg[j].fraseincumplimientoBas,reg[j].fraseincumplimientoVoc1,reg[j].fraseincumplimientoVoc2,reg[j].fecharec);
		 }
		if (strcmp(reg[j].miniincumplimientos,"\0")!=0){fprintf(reporteEkuis,"*punto %d (%s) miniincumplimiento lims %d por los test: %s fecha %s \n",reg[j].ekuis,reg[j].nombre,reg[j].referencia,reg[j].miniincumplimientos,reg[j].fecharec);
		strcpy(reg[j].obscali,reg[j].miniincumplimientos);
		 }
	 }
	  	
	

//crea el nombre del archivo xml
	strcpy(ruta,"MuestrasEkuis");

	strcpy(texto,reg[min].fechaaut);
	token=texto;

    for (j=0;j<7;j++){	
     if(	*(token+j)=='/') {
	 *(token+j)='-';
    }}

	strcat(ruta,token);
	strcat(ruta,"_");
	strcpy(texto,reg[max].fechaaut);

	token=texto;

     for (j=0;j<7;j++){	
	     if(	*(token+j)=='/') {
		 *(token+j)='-';
	 }}
	       
	strcat(ruta,token);
	strcat(ruta,".xml");
    
    xml=fopen(ruta,"w");

    if (xml==NULL){printf("Error al escribir en archivo\n");}


// generacion del fichero XML y el de reporte.txt

	 
   	fprintf(reporteEkuis,"\n ***Se han generado las siguientes muestras: ");
	for (j=0;j<numanalisis;j++){
		fprintf(reporteEkuis," %d ,",reg[j].referencia);
		if (j%10==0){fprintf(reporteEkuis,"\n");	}
	}


   fputs("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",xml);
   fprintf(reporteEkuis,"\n");
   fputs("<laboratorio codigo=\"DEMSAC\">\n",xml);
    
   for (i=0;i<numanalisis;i++){
        	
        	if (strlen(reg[i].obscali)>100){printf("\n ******* ERROR, El campo observaciones de calificacion no puede tener mas de 100 caracteres ******\n"); getchar(); getchar();exit(-1);
			}
			
        	if (strlen(reg[i].obs)>100){printf("\n ******* ERROR, El campo observaciones de laboratorio no puede tener mas de 100 caracteres ******\n"); getchar();getchar();exit (-1);
			}
			
        	// poner cero a la izquierda en num de ptio muestreo ekuis
        	
       	itoa(reg[i].ekuis,texto,10);
        
        if (strlen(texto)==10){strcpy(temp,"0");
        strcat(temp,texto);

		}else { strcpy(temp,texto);
		}
    
  	 fprintf(xml,"<muestra referencia=\"%d\" punto=\"%s\" tipo=\"%s\" motivo=\"3\" calificacion=\"%d\" obscal=\"%s\" fecharec=\"%s\" fechaana=\"%s\" obslab=\"%s\" > \n",reg[i].referencia, temp,  reg[i].tipo,reg[i].calificacion,reg[i].obscali, reg[i].fecharec,reg[i].fecharec,reg[i].obs);
   	 if (reg[i].calificacion==3){fprintf(reporteEkuis,"\n---------------------------------------------------SIN CALIFICACION----------------------------------------------------------------------\n");		}
	 if (reg[i].calificacion==2){fprintf(reporteEkuis,"\n**************************************************NO APTA************************************************************************\n");		}
	 if (reg[i].calificacion==4){fprintf(reporteEkuis,"\n--------------------------------------------------CUMPLE VP PERO SIN PARAMETROS SUFICIENTES PARA VALORACION-----------------------------------------------------------------------\n");		}
	
	
	fprintf(reporteEkuis,"---------------------------------------------------------------------------------------------------------------------------------------------\n");
	fprintf(reporteEkuis,"-->LIMS %d  punto %d (%s) tipo %s  motivo %d  calif %d  obscal: %s  fecha %s  obs:%s.\n",reg[i].referencia, reg[i].ekuis, reg[i].nombre, reg[i].tipo,  reg[i].motivo,reg[i].calificacion,reg[i].obscali, reg[i].fecharec,reg[i].obs);
   	
   	for (j=0;j<numanalisis;j++){ // imprime la analitica basica

           if ( analisis[j].idlims==reg[i].referencia!=0){	
           	
           	
           	    if (strcmp(analisis[j].Tcoto,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"COTO\" ensayo=\"PNTE-MI-016\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"UFC/100 ml\"/>\n",analisis[j].Tcoto,analisis[j].Vcoto);
       			fprintf(reporteEkuis,"coliformes tot %s%d UFC/100ml\n",simbolo(analisis[j].Tcoto),analisis[j].Vcoto);
			   	}
			   	
			    if (strcmp(analisis[j].Tecoli,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"ECOLI\" ensayo=\"PNTE-MI-016\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"UFC/100 ml\"/>\n",analisis[j].Tecoli,analisis[j].Vecoli);
       			fprintf(reporteEkuis,"e coli %s%d UFC/100ml\n",simbolo(analisis[j].Tecoli),analisis[j].Vecoli);
			   	}
			   	
			   	if (strcmp(analisis[j].Tturb,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"TURB\" ensayo=\"PNTE-AI-008\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"UNF\"/>\n",analisis[j].Tturb, analisis[j].Vturb);
       			fprintf(reporteEkuis,"turbidez %s%.2f UNF\n",simbolo(analisis[j].Tturb),analisis[j].Vturb);
			   	}
			   	
			    if (strcmp(analisis[j].Tamon,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"AMON\" ensayo=\"PNTE-AI-004\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tamon,analisis[j].Vamon);
       			fprintf(reporteEkuis,"amonio %s%.2f mg/l\n",simbolo(analisis[j].Tamon),analisis[j].Vamon);
			   	}
			   	
		   	    if (strcmp(analisis[j].Tph,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"PH\" ensayo=\"PNTC-AI-019\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"-\"/>\n",analisis[j].Tph,analisis[j].Vph);
       				fprintf(reporteEkuis,"pH %s%.1f\n",simbolo(analisis[j].Tph),analisis[j].Vph);
			    }
				   
				if (strcmp(analisis[j].Tsulf,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"SULF\" ensayo=\"PNTC-AI-005\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tsulf,analisis[j].Vsulf);
       			fprintf(reporteEkuis,"sulfatos %s%.2f mg/l\n",simbolo(analisis[j].Tsulf),analisis[j].Vsulf);
			   	} 
				    	 			   	
	            if (strcmp(analisis[j].Tconduc,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"CONDUC\" ensayo=\"PNTE-AI-001\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"&#181;S/cm\"/>\n",analisis[j].Tconduc,analisis[j].Vconduc);
       			fprintf(reporteEkuis,"Conductividad %s%d µS/cm\n",simbolo(analisis[j].Tconduc),analisis[j].Vconduc);
				}  

		   	    if (strcmp(analisis[j].Tf,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"F\" ensayo=\"PNTE-AI-005\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tf,analisis[j].Vf);
       				fprintf(reporteEkuis,"fluor %s%.2f mg/l\n",simbolo(analisis[j].Tf),analisis[j].Vf);
				}
				
		   	    if (strcmp(analisis[j].Tclo,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"CLO\" ensayo=\"PNTE-AI-005\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tclo,analisis[j].Vclo);
       			fprintf(reporteEkuis,"cloruros %s%.2f mg/l\n",simbolo(analisis[j].Tclo),analisis[j].Vclo);
			   	}
			   	
		   	    if (strcmp(analisis[j].Tnitra,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"NITRA\" ensayo=\"PNTE-AI-005\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tnitra,analisis[j].Vnitra);
       			fprintf(reporteEkuis,"nitrato %s%.2f mg/l\n",simbolo(analisis[j].Tnitra),analisis[j].Vnitra);
			   	}
			   	
		   	    if (strcmp(analisis[j].Tnitri,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"NITRI\" ensayo=\"PNTE-AI-005\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Tnitri,analisis[j].Vnitri);
       				fprintf(reporteEkuis,"nitrito %s%.2f mg/l\n",simbolo(analisis[j].Tnitri),analisis[j].Vnitri);
				}
				
	  	 	    if (strcmp(analisis[j].Tcl2lib,"E")!=0){
	  	 	    	if(analisis[j].Vcl2lib>1.5){	fprintf(xml,"<parametro codigo=\"CL2LIB\" ensayo=\"PNTE-LC-001\" tiporesultado=\"G\" resultado=\"1.5\" unimed=\"mg/l\"/>\n");
					   }else{	fprintf(xml,"<parametro codigo=\"CL2LIB\" ensayo=\"PNTE-LC-001\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"mg/l\"/>\n",analisis[j].Tcl2lib,analisis[j].Vcl2lib);
			        }
       			fprintf(reporteEkuis,"cloro libre %s%.1f mg/l\n",simbolo(analisis[j].Tcl2lib),analisis[j].Vcl2lib);
			   	}
			   	
		   	    if (strcmp(analisis[j].Tcl2cob,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"CL2COB\" ensayo=\"PNTE-LC-001\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"mg/l\"/>\n",analisis[j].Tcl2cob,analisis[j].Vcl2cob);
       			fprintf(reporteEkuis,"cloro combinado %s%.1f mg/l\n",simbolo(analisis[j].Tcl2cob),analisis[j].Vcl2cob);
			   	}
			   	
			   	
			   	if (strcmp(analisis[j].Tentco,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"ENTCO\" ensayo=\"PNTE-MI-013\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"UFC/100 ml\"/>\n",analisis[j].Tentco,analisis[j].Ventco);
       			fprintf(reporteEkuis,"enterococos %s%d UFC/100ml\n",simbolo(analisis[j].Tentco),analisis[j].Ventco);
				}
				      
	  	 		if (strcmp(analisis[j].Tbh22,"E")!=0){
	  	 		fprintf(reporteEkuis,"aerobios %s%d UFC/ml\n",simbolo(analisis[j].Tbh22),analisis[j].Vbh22);
	  	 			if(analisis[j].Vbh22==0){fprintf(xml,"<parametro codigo=\"BH22\" ensayo=\"PNTE-MI-068\" tiporesultado=\"L\" resultado=\"1\" unimed=\"UFC/ml\"/>\n");
					   }else{	  
            	fprintf(xml,"<parametro codigo=\"BH22\" ensayo=\"PNTE-MI-068\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"UFC/ml\"/>\n",analisis[j].Tbh22,analisis[j].Vbh22);
       			}}

	  	 	    if (strcmp(analisis[j].Tclper,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"CLPER\" ensayo=\"PNTE-MI-030\" tiporesultado=\"%s\" resultado=\"%d\" unimed=\"UFC/100 ml\"/>\n",analisis[j].Tclper,analisis[j].Vclper);
       				fprintf(reporteEkuis,"clostridios per.  %s%d UFC/100ml\n",simbolo(analisis[j].Tclper),analisis[j].Vclper);
				}
				     
				if (strcmp(analisis[j].Toxi,"E")!=0){
            	fprintf(xml,"<parametro codigo=\"OXI\" ensayo=\"PNTE -AI-006\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"mg/l\"/>\n",analisis[j].Toxi,analisis[j].Voxi);
       				fprintf(reporteEkuis,"oxidabilidad %s%.2f mg/l\n",simbolo(analisis[j].Toxi),analisis[j].Voxi);
				}
	  	 	   
		      } 
	   } //end del for de cada analitica basica
	
       

       	  for (j=0;j<numanalisismetales;j++){ // imprime la analitica metales

           if ( Analisismetales[j].idlims==reg[i].referencia!=0){
    
    		   	if (strcmp(Analisismetales[j].Tal,"E")!=0){ fprintf(reporteEkuis,"Aluminio.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tal),Analisismetales[j].Val);
				   	fprintf(xml,"<parametro codigo=\"AL\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tal,Analisismetales[j].Val);
       			}
				   	
       			if (strcmp(Analisismetales[j].Tsb,"E")!=0){fprintf(reporteEkuis,"Antimonio. %s %.2f µg/l\n",simbolo(Analisismetales[j].Tsb),Analisismetales[j].Vsb);
            	fprintf(xml,"<parametro codigo=\"SB\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tsb,Analisismetales[j].Vsb);
       			}
       			
       			if (strcmp(Analisismetales[j].Tas,"E")!=0){fprintf(reporteEkuis,"Arsenico.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tas),Analisismetales[j].Vas);
            	fprintf(xml,"<parametro codigo=\"AS\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tas,Analisismetales[j].Vas);
       			}
       			
       			if (strcmp(Analisismetales[j].Tba,"E")!=0){fprintf(reporteEkuis,"Bario.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tba),Analisismetales[j].Vba);
            	fprintf(xml,"<parametro codigo=\"BA\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tba,Analisismetales[j].Vba);
       			}
       			
		   	    if (strcmp(Analisismetales[j].Tb,"E")!=0){ //hay que limitar el numero de decimales en boro en el xml
		   	    		if (Analisismetales[j].Vb<0.091){
						     	fprintf(reporteEkuis,"Boro.  <0.09 mg/l\n");
						   	fprintf(xml,"<parametro codigo=\"B\" ensayo=\"PNTE-AI-015\" tiporesultado=\"L\" resultado=\"0.09\" unimed=\"mg/l\"/>\n");
					   }else{
					   	fprintf(reporteEkuis,"Boro.  %s%.2f mg/l\n",simbolo(Analisismetales[j].Tb),Analisismetales[j].Vb); 
            	fprintf(xml,"<parametro codigo=\"B\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.3f\" unimed=\"mg/l\"/>\n",Analisismetales[j].Tb,Analisismetales[j].Vb);
       			}}

		   		if (strcmp(Analisismetales[j].Tcd,"E")!=0){fprintf(reporteEkuis,"Cadmio.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tcd),Analisismetales[j].Vcd);
            	fprintf(xml,"<parametro codigo=\"CD\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tcd, Analisismetales[j].Vcd);
       			}
       			
	  	 	    //para el cobre se han cambiado unidades y ademas hay que limitar el numero de decimales para el xml
			    if (strcmp(Analisismetales[j].Tcu,"E")!=0){
	  	 	    	if (Analisismetales[j].Vcu<0.091){	fprintf(xml,"<parametro codigo=\"CU\" ensayo=\"PNTE-AI-015\" tiporesultado=\"L\" resultado=\"0.09\" unimed=\"mg/l\"/>\n");
	  	 	    	fprintf(reporteEkuis,"Cobre.  <0.09 mg/l\n");
					   }else{
					  fprintf(reporteEkuis,"Cobre.  %s%.2f mg/l\n",simbolo(Analisismetales[j].Tcu),Analisismetales[j].Vcu); 	   
            	fprintf(xml,"<parametro codigo=\"CU\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.3f\" unimed=\"mg/l\"/>\n",Analisismetales[j].Tcu,Analisismetales[j].Vcu);
       			}}
       			
		   	   	if (strcmp(Analisismetales[j].Tcr,"E")!=0){fprintf(reporteEkuis,"cromo. %s %.2f µg/l\n",simbolo(Analisismetales[j].Tcr),Analisismetales[j].Vcr);
            	fprintf(xml,"<parametro codigo=\"CR\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tcr,Analisismetales[j].Vcr);
       			}
				  
				   if (strcmp(Analisismetales[j].Tfe,"E")!=0){fprintf(reporteEkuis,"Hierro. %s %.2f µg/l\n",simbolo(Analisismetales[j].Tfe),Analisismetales[j].Vfe);
            	fprintf(xml,"<parametro codigo=\"FE\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tfe,Analisismetales[j].Vfe);
       			}
       					
       			      			
		   		if (strcmp(Analisismetales[j].Tmn,"E")!=0){fprintf(reporteEkuis,"Manganeso.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tmn),Analisismetales[j].Vmn);
            	fprintf(xml,"<parametro codigo=\"MN\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tmn,Analisismetales[j].Vmn);
       			}
       			
       		  	if (strcmp(Analisismetales[j].Tmo,"E")!=0){fprintf(reporteEkuis,"Molibdeno.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tmo),Analisismetales[j].Vmo);
            	fprintf(xml,"<parametro codigo=\"MO\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tmo,Analisismetales[j].Vmo);
       			}		
 
	  	 	    if (strcmp(Analisismetales[j].Tni,"E")!=0){fprintf(reporteEkuis,"Niquel.  %s%.2f µg/l\n",simbolo(Analisismetales[j].Tni),Analisismetales[j].Vni);
            	fprintf(xml,"<parametro codigo=\"NI\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tni,Analisismetales[j].Vni);
       			}
       			
	  	 	    if (strcmp(Analisismetales[j].Tpb,"E")!=0){fprintf(reporteEkuis,"Plomo. %s %.2f µg/l\n",simbolo(Analisismetales[j].Tpb),Analisismetales[j].Vpb);
            	fprintf(xml,"<parametro codigo=\"PB\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tpb,Analisismetales[j].Vpb);
       			}
       			
	  	 		if (strcmp(Analisismetales[j].Tse,"E")!=0){fprintf(reporteEkuis,"Selenio. %s %.2f µg/l\n",simbolo(Analisismetales[j].Tse),Analisismetales[j].Vse);
            	fprintf(xml,"<parametro codigo=\"SE\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tse,Analisismetales[j].Vse);
       			}

	  	 	    if (strcmp(Analisismetales[j].Tzn,"E")!=0){fprintf(reporteEkuis,"Zinc. %s%.2f µg/l\n",simbolo(Analisismetales[j].Tzn),Analisismetales[j].Vzn);
            	fprintf(xml,"<parametro codigo=\"ZN\" ensayo=\"PNTE-AI-015\" tiporesultado=\"%s\" resultado=\"%.2f\" unimed=\"&#181;g/l\"/>\n",Analisismetales[j].Tzn,Analisismetales[j].Vzn);
       			}
   }
} // fin analitica metales



// Analitica VOCS 1


	  for (j=0;j<numanalisisvoc1;j++){ 
		
           if ( voc[j].idlims==reg[i].referencia!=0){
           		if (strcmp(voc[j].Tc4c2,"E")!=0){fprintf(reporteEkuis,"C4C2. %s %.1f µg/l\n",simbolo(voc[j].Tc4c2),voc[j].Vc4c2);
            	fprintf(xml,"<parametro codigo=\"C4C2\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tc4c2,voc[j].Vc4c2);
       			}
            	if (strcmp(voc[j].T112tce,"E")!=0){fprintf(reporteEkuis,"112TCE.  %s%.1f µg/l\n",simbolo(voc[j].T112tce),voc[j].V112tce);
            	fprintf(xml,"<parametro codigo=\"112TCE\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].T112tce,voc[j].V112tce);
       			}
            	if (strcmp(voc[j].T12dce,"E")!=0){fprintf(reporteEkuis,"12DCE.  %s%.1f µg/l\n",simbolo(voc[j].T12dce),voc[j].V12dce);
            	fprintf(xml,"<parametro codigo=\"12DCE\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].T12dce,voc[j].V12dce);
       			}
       	    	if (strcmp(voc[j].T12dcp,"E")!=0){fprintf(reporteEkuis,"12DCP. %s %.1f µg/l\n",simbolo(voc[j].T12dcp),voc[j].V12dcp);
            	fprintf(xml,"<parametro codigo=\"12DCP\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].T12dcp,voc[j].V12dcp);
       			}
    			if (strcmp(voc[j].T13dcb,"E")!=0){fprintf(reporteEkuis,"13dcb. %s %.1f µg/l\n",simbolo(voc[j].T13dcb),voc[j].V13dcb);
            	fprintf(xml,"<parametro codigo=\"13DCB\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].T13dcb,voc[j].V13dcb);
       			}
            	if (strcmp(voc[j].T14dcb,"E")!=0){fprintf(reporteEkuis,"14dcb.  %s%.1f µg/l\n",simbolo(voc[j].T14dcb),voc[j].V14dcb);
            	fprintf(xml,"<parametro codigo=\"14DCB\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].T14dcb,voc[j].V14dcb);
       			}
       			if (strcmp(voc[j].Tben,"E")!=0){fprintf(reporteEkuis,"benceno. %s %.1f µg/l\n",simbolo(voc[j].Tben),voc[j].Vben);
            	fprintf(xml,"<parametro codigo=\"BEN\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tben,voc[j].Vben);
       			}
            	if (strcmp(voc[j].Tbdcm,"E")!=0){fprintf(reporteEkuis,"Bromodiclorometano. %s %.1f µg/l\n",simbolo(voc[j].Tbdcm),voc[j].Vbdcm);
            	fprintf(xml,"<parametro codigo=\"BDCM\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tbdcm,voc[j].Vbdcm);
       			}
            	if (strcmp(voc[j].Tbr3ch,"E")!=0){fprintf(reporteEkuis,"bromoformo. %s %.1f µg/l\n",simbolo(voc[j].Tbr3ch),voc[j].Vbr3ch);
            	fprintf(xml,"<parametro codigo=\"BR3CH\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tbr3ch,voc[j].Vbr3ch);
       			}		
       			if (strcmp(voc[j].Tc13dcp,"E")!=0){fprintf(reporteEkuis,"c13DCP.  %s%.1f µg/l\n",simbolo(voc[j].Tc13dcp),voc[j].Vc13dcp);
            	fprintf(xml,"<parametro codigo=\"C13CDP\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tc13dcp,voc[j].Vc13dcp);
       			}
			    if (strcmp(voc[j].Tcb,"E")!=0){fprintf(reporteEkuis,"clorobenceno. %s %.1f µg/l\n",simbolo(voc[j].Tcb),voc[j].Vcb);
            	fprintf(xml,"<parametro codigo=\"CB\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tcb,voc[j].Vcb);
       			} 	
                if (strcmp(voc[j].Tcl3ch,"E")!=0){fprintf(reporteEkuis,"cloroformo.  %s%.1f µg/l\n",simbolo(voc[j].Tcl3ch),voc[j].Vcl3ch);
            	fprintf(xml,"<parametro codigo=\"CL3CH\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tcl3ch,voc[j].Vcl3ch);
       			}
       			if (strcmp(voc[j].Tclvi,"E")!=0){fprintf(reporteEkuis,"Cloruro vinilo.  %s%.1f µg/l\n",simbolo(voc[j].Tclvi),voc[j].Vclvi);
            	fprintf(xml,"<parametro codigo=\"CLVI\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tclvi,voc[j].Vclvi);
       			}
       			    			
}  // end del if	

}// end del for  


// Analitica VOCS 2



	  for (j=0;j<numanalisisvoc2;j++){ 
	
           if ( voc[j].idlims==reg[i].referencia!=0){
           	
           	
           	
           	// descomentar cuando arreglen lo del metodo de ensayo de estos cuatro parametros
      		/*
           		if (strcmp(voc[j].Tald,"E")!=0){fprintf(reporteEkuis,"Aldrin. %s %.1f ng/l\n",simbolo(voc[j].Tald),voc[j].Vald);
            	fprintf(xml,"<parametro codigo=\"ALD\" ensayo=\"PNTE-AI-042\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"ng/l\"/>\n",voc[j].Tald,voc[j].Vald);
       			}	
       			if (strcmp(voc[j].Tdiel,"E")!=0){fprintf(reporteEkuis,"Dieldrin. %s %.1f ng/l\n",simbolo(voc[j].Tdiel),voc[j].Vdiel);
            	fprintf(xml,"<parametro codigo=\"DIEL\" ensayo=\"PNTE-AI-042\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"ng/l\"/>\n",voc[j].Tdiel,voc[j].Vdiel);
       			}
       		   
				if (strcmp(voc[j].Thpcl,"E")!=0){fprintf(reporteEkuis,"Heptacloro. %s %.1f ng/l\n",simbolo(voc[j].Thpcl),voc[j].Vhpcl);
            	fprintf(xml,"<parametro codigo=\"HPCL\" ensayo=\"PNTE-AI-042\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"ng/l\"/>\n",voc[j].Thpcl,voc[j].Vhpcl);
       			}	
       			if (strcmp(voc[j].Thpcle,"E")!=0){fprintf(reporteEkuis,"Heptacloro epoxi. %s %.1f ng/l\n",simbolo(voc[j].Thpcle),voc[j].Vhpcle);
            	fprintf(xml,"<parametro codigo=\"HPCLE\" ensayo=\"PNTE-AI-042\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"ng/l\"/>\n",voc[j].Thpcle,voc[j].Vhpcle);
       			}  
       		
			*/   
			   
			   
			   
			
       			//plaguicidas totales se han cambiado las unidades de nanogramos a microgramos
				if (strcmp(voc[j].Tpltot,"E")!=0){
					if (voc[j].Vpltot<0.091) {fprintf(reporteEkuis,"Plaguicidas totales. <0.09 µg/l\n");
						fprintf(xml,"<parametro codigo=\"PLTOT\" ensayo=\"PNTE-AI-042 \" tiporesultado=\"L\" resultado=\"0.09\" unimed=\"&#181;g/l\"/>\n",voc[j].Tpltot,voc[j].Vpltot); }else
						{
						 fprintf(reporteEkuis,"Plaguicidas totales. %s %.1f µg/l\n",simbolo(voc[j].Tpltot),voc[j].Vpltot);			   	
                         fprintf(xml,"<parametro codigo=\"PLTOT\" ensayo=\"PNTE-AI-042\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tpltot,voc[j].Vpltot);
       			}  }
				  
				if (strcmp(voc[j].Tthm,"E")!=0){fprintf(reporteEkuis,"Trihalometanos. %s %.1f µg/l\n",simbolo(voc[j].Tthm),voc[j].Vthm);
            	fprintf(xml,"<parametro codigo=\"THM\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tthm,voc[j].Vthm);
       			}  
			
       			if (strcmp(voc[j].Ttol,"E")!=0){fprintf(reporteEkuis,"Tolueno.  %s%.1f µg/l\n",simbolo(voc[j].Ttol),voc[j].Vtol);
            	fprintf(xml,"<parametro codigo=\"TOL\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Ttol,voc[j].Vtol);
       			}
				  
				if (strcmp(voc[j].Tetb,"E")!=0){fprintf(reporteEkuis,"etilbenceno. %s %.1f µg/l\n",simbolo(voc[j].Tetb),voc[j].Vetb);
            	fprintf(xml,"<parametro codigo=\"ETB\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tetb,voc[j].Vetb);
       			}	

       			if (strcmp(voc[j].Tclet,"E")!=0){fprintf(reporteEkuis,"CLET. %s %.1f µg/l\n",simbolo(voc[j].Tclet),voc[j].Vclet);
            	fprintf(xml,"<parametro codigo=\"CLET\" ensayo=\"PNTE-AI-037\" tiporesultado=\"%s\" resultado=\"%.1f\" unimed=\"&#181;g/l\"/>\n",voc[j].Tclet,voc[j].Vclet);
       			}

}  // end del if	

}// end del for  

	
 	fputs("</muestra>\n",xml);  	
   	
}	 // fin del archivo xml		
  
    
    fputs("</laboratorio>",xml);


	if (fclose(xml)==0){
		printf("\nSe cerro el fichero correctamente. Ver los ficheros XML para enviar a Ekuis y el txt para registro\n");
	   	printf("Ayuntamiento de Vitoria, Laboratorio Municipal\n ");
	}
		

free (reg);
free (analisis);
free (Analisismetales);
free (voc);


   
   fprintf(reporteEkuis,"Fin reporte EKUIS, Ayunta de Vitoria, Labo Municipal.");
   if (fclose(reporteEkuis)!=0){printf("problema al cerrar el fichero de reporte.");
   }


	return 0;
} // end of main

