import graph;
usepackage("mathrsfs");

// Structure fonction 
struct fonction {
	string type; // type : cartésienne, polaire, paramétrique
	real f(real); // définition de la fonction cartésienne
	real x(real); // définition de la fonction paramétrique
	real y(real); // définition de la fonction paramétrique
	real rho(real); // définition de la fonction polaire
	real[] min,max; // intervalles de la variable
	int n; // nombre de points à calculer
	pen couleur_fonction; // couleur (dark)red,blue,green,magenta,cyan,olive,brown,gray
	pen epaisseur_fonction; // épaisseur 1bp, ...
	pen trait_fonction; //trait solid,dotted,dashed,longdashed,dashdotted,longdashdotted
	string legende; // légende de la courbe
	real position_legende; // paramètre de positionnement de la légende
	align alignement_legende; // S,N,E,W,NE,NW,SE,SW,...
	pen couleur_legende; // couleur de la légende
	real[] parametres_points; // paramètres des points sur la courbe
	string[] legendes_points; // noms des points sur la courbe
	align[] alignements_points; // S,N,E,W,NE,NW,SE,SW,...
	real[] diametres_points; // diamètre du disque
	pen[] couleurs_points; // couleur (dark)red,blue,green,magenta,cyan,olive,brown,gray
	real[] parametres_tangentes; // paramètres (x,t ou thêta) des tangentes à la courbe séparés par une virgule
	pen[] couleurs_tangentes; // couleurs des points séparés par une virgule
	pen[] epaisseurs_tangentes; // épaisseur des tangentes
	real[] directions_tangentes; // 1 : sens de parcours, 2 : des deux côtés
	real[] coefficient_tangentes; // coef de réduction ou d'agrandissement
	real coef_label_fct; // Coefficient taille des caractères
}

real nb_derive(real f(real),real x, real h=1/10^6){
	// renvoie le nombre dérivé de la fonction f en x
	return (f(x+h)-f(x))/h;
}

// Procédures issues de base_pi
struct rational
{/*'p' est le numérateur, 'q' est le dénominateur.
   'ep' est la précision avec laquelle le rationnel a été obtenu dans
   le cas où il y convertion à partir d'un irrationnel.
  */
  int p;
  int q;
  real ep;
}

rational operator init() {return new rational;}
rational rational(real x, real ep=1/10^5)
{/*Retourne le rationnel qui approxime 'x' tel que 'abs(p/q-x)<=ep'.
 */
  rational orat;
  int q=1;
  while (abs(round(q*x)-q*x)>ep)
    {
      ++q;
    }
  orat.p=round(q*x);
  orat.q=q;
  orat.ep=ep;
  return orat;
}

int pgcd(int a, int b)
{
  int a_=abs(a), b_=abs(b), r=a_;
  if (b_>a_) {a_=b_; b_=r; r=a_;}
  while (r>0)
    {
      r=a_%b_;
      a_=b_;
      b_=r;
    }
  return a_;
}

string texfrac(int p, int q,
               string factor="",
               bool signin=false, bool factorin=true,
               bool displaystyle=false,
               bool zero=true)
{/*Retourne le code LaTeX pour écrire la fraction p/q*factor.
   Si 'signin' vaut 'true' le signe '-' est dans la fraction (au
   numérateur).
   Si 'displaystyle' vaut 'true' le code est en mode 'displaystyle'.
   Si 'zero' vaut 'false' et 'p' vaut 0, le code génère 0/p*factor; 0
   si 'zero' vaut 'true'.
  */
  if (p==0) return (zero ? "$0$" : "");
  string disp= displaystyle ? "$\displaystyle " : "$";
  int pgcd=pgcd(p,q);
  int num= round(p/pgcd), den= round(q/pgcd);
  string nums;
  if (num==1)
    if (factor=="" || (!factorin && (den !=1))) nums="1"; else nums="";
  else
    if (num==-1)
      if (factor=="" || (!factorin && (den !=1))) nums="-1"; else nums="-";
    else nums= (string) num;
  if (den==1) return "$" + nums + factor + "$";
  else
    {
      string dens= (den==1) ? "" : (string) den;
      if (signin || num>0)
        if (factorin)
          return disp + "\frac{" + nums + factor + "}{" + (string) dens + "}$";
        else
          return disp + "\frac{" + nums + "}{" + (string) dens + "}"+ factor + "$";
      else
        {
          if (num==-1)
            if (factor=="" || !factorin) nums="1"; else nums="";
          else nums=(string)(abs(num));
          if (factorin)
            return disp + "-\frac{" + nums + factor + "}{" + (string) dens + "}$";
          else
            return disp + "-\frac{" + nums + "}{" + (string) dens + "}"+ factor + "$";
        }
    }
}

string texfrac(rational x,
               string factor="",
               bool signin=false, bool factorin=true,
               bool displaystyle=false,
               bool zero=true)
{
  return texfrac(x.p, x.q, factor, signin, factorin, displaystyle, zero);
}

ticklabel labelfrac(real ep=1/10^5, real factor=1.0,
                    string symbol="",
                    bool signin=false, bool symbolin=true,
                    bool displaystyle=false,
                    bool zero=true)
{
  return new string(real x)
    {
      return texfrac(rational(x/factor), symbol, signin, symbolin, displaystyle, zero);
    };
}
////////////////////////////////////////////////////////////////////////

fonction[] fonctions; // tableau des fonctions à tracer

// Taille du dessin
unitsize(5cm,5cm); // ou size(10cm);
// Taille de la fenêtre
real xmin=-1,xmax=2.1; 
real ymin=-2,ymax=2;
// Propriétés du graphique /////////////////////////////////////////////
bool quadrillage=true; // affiche le quadrillage ou non
bool xaxe=true; // affiche l'axe horizontal ou non
real xPasPrincipal=0.5; // pas principal de l'axe horizontal
real xPasSecondaire=0.25; // pas secondaire de l'axe horizontal
int xNbChiffres=2 ; // nombre de chiffres pour les graduations. 0=valeur exacte.
real xfactor=pi; string xsymbol="\pi"; // pris en compte seulement si xNbChiffres=0
string legende_x="$x$"; // légende de l'axe horizontal
real coef_labelx=0.7; // Coefficient taille du label x
bool yaxe=true; // affiche l'axe vertical ou non
real yPasPrincipal=0.5; // pas principal de l'axe vertical
real yPasSecondaire=0.25; // pas secondaire de l'axe vertical
int yNbChiffres=2; // nombre de chiffres pour les graduations. 0=valeur exacte.
real yfactor=1; string ysymbol=""; // pris en compte seulement si yNbChiffres=0
string legende_y="$y$"; // légende de l'axe vertical
real coef_labely=0.7; // Coefficient taille du label y
bool repereoij=false; // affiche O et les vecteurs i et j

// Création du dessin
draw(box((xmin,ymin),(xmax,ymax)),invisible);

// Une fonction à tracer ///////////////////////////////////////////////
fonction fct;
fct.type="pol"; // type d'équation (car (cartésienne), pol (polaire), par (paramétrique))
fct.f=new real(real x) {return x^2;}; // cartésienne
fct.x=new real(real t) {return sin(2*t);}; // paramétrique
fct.y=new real(real t) {return sin(3*t);}; // paramétrique
fct.rho=new real(real theta) {return 1+cos(theta);}; // polaire
fct.min=new real[] {0}; // bornes inf de l'intervalle de la variable (x,t ou thêta)
fct.max=new real[] {2*pi}; // bornes sup de l'intervalle de la variable (x,t ou thêta)
fct.n=400; // nombre de points à calculer
fct.couleur_fonction=darkmagenta; // couleur ( (dark)red,blue,green,magenta,cyan,olive,brown,gray)
fct.epaisseur_fonction=linewidth(0.75bp); // épaisseur (0.75bp)
fct.trait_fonction=solid; //trait (solid,dotted,dashed,longdashed,dashdotted,longdashdotted)
fct.position_legende=pi/8; // paramètre (x,t ou thêta) de positionnement de la légende 
fct.legende="$\mathscr{C}$"; // légende de la courbe ("$\mathscr{C}$")
fct.alignement_legende=N; // alignement de la légende (S,N,E,W,NE,NW,SE,SW,...)
fct.couleur_legende=fct.couleur_fonction; // couleur de la légende
fct.parametres_points=new real[] {pi/6,pi/2}; // paramètres (x,t ou thêta) des points sur la courbe séparés par une virgule
fct.legendes_points=new string[] {"$M$","$N$"}; // legendes des points séparés par une virgule
fct.alignements_points=new align[] {E,NW}; // alignement des points séparés par une virgule
fct.diametres_points=new real[] {3bp}; // diamètres des points séparés par une virgule
fct.couleurs_points=new pen[] {fct.couleur_fonction}; // couleurs des points séparés par une virgule
fct.parametres_tangentes=new real[] {pi/6,0,pi/2}; // paramètres (x,t ou thêta) des tangentes à la courbe séparés par une virgule
fct.couleurs_tangentes=new pen[] {fct.couleur_fonction+0.5*white}; // couleurs des tangentes séparées par une virgule
fct.directions_tangentes=new real[] {1}; // 1 : sens de parcours, 2 : des deux côtés
fct.coefficient_tangentes=new real[] {0.5,0.5}; // coef de réduction ou d'agrandissement
fct.epaisseurs_tangentes=new pen[] {linewidth(0.5bp)}; // épaisseurs des tangentes
fct.coef_label_fct=0.8; // coefficient taille des caractères
fonctions.push(fct);
// fin de la fonction à tracer /////////////////////////////////////////

// Points à placer /////////////////////////////////////////////////////
pair[] coordonnees_points={(0,0),(1,0.5)}; // coordonnées des points séparés par une virgule ((1,2),(2,-1),...)
string[] legendes_points={"$A$","$C$"}; // légendes des points séparés par une virgule ("$A$","$B$",...)
align[] alignements_points={NE}; // alignement des légendes séparés par une virgule (S,N,E,W,NE,NW,SE,SW,...) 
real[] diametres_points={3bp}; // diamètre des points séparés par une virgule (3bp)
pen[] couleurs_points={black}; // couleurs des points séparés par une virgule ((dark)red,blue,green,magenta,cyan,olive,brown,gray)
real coef_label_points=0.8; // coefficient taille des caractères
// fin des points à placer /////////////////////////////////////////////

if (quadrillage) {
// Quadrillage horizontal //////////////////////////////////////////////
ticks yticks=Ticks("%",extend=true,
		Step=yPasPrincipal,Size=.6mm, // graduation principale
		step=yPasSecondaire,size=.4mm, // graduation secondaire
		pTick=paleblue+.2bp,
		ptick=dotted+.5bp+paleblue
	);
yaxis(ymin=ymin,ymax=ymax,
	LeftRight,
	yticks, 
	above=false,
	p=nullpen
); 

// Quadrillage vertical ////////////////////////////////////////////////
ticks xticks=Ticks("%",extend=true,
		Step=xPasPrincipal,Size=.6mm, // graduation principale
		step=xPasSecondaire,size=.4mm, // graduation secondaire
		pTick=paleblue+.2bp,
		ptick=dotted+.5bp+paleblue
	);
xaxis(xmin=xmin,xmax=xmax,
	BottomTop,
	xticks, 
	above=false,
	p=nullpen
); 
}
//write(max(currentpicture));

if(xaxe) {
// Axes x //////////////////////////////////////////////////////////////
string xFormat=(xNbChiffres==0 ? "" : "$%."+string(xNbChiffres)+"g$");
ticks xticks=Ticks(
		scale(coef_labelx)*Label(xFormat,align=E,Fill(white)),
		(xNbChiffres==0 ? labelfrac(factor=xfactor,symbol=xsymbol) : null),
		NoZero, // afficher le zéro
		begin=true,beginlabel=true, // Affiche première graduation
		end=false,endlabel=false, // Affiche dernière graduation
		Step=xPasPrincipal,Size=.6mm,pTick=black, // Pas des graduations principales
		step=xPasSecondaire,size=.4mm,ptick=gray // Pas des graduations secondaires
	);
xaxis(scale(coef_labelx+0.2)*Label(legende_x,position=EndPoint, align=2E+N),
	xmin=xmin,xmax=xmax,
	xticks, 
	Arrow(6),
	above=true
);
}

if(yaxe) {
// Axes y //////////////////////////////////////////////////////////////
string yFormat=(yNbChiffres==0 ? "" : "$%."+string(yNbChiffres)+"g$");
ticks yticks=Ticks(
		scale(coef_labely)*Label(yFormat,align=E,Fill(white)),
		(yNbChiffres==0 ? labelfrac(factor=yfactor,symbol=ysymbol) : null),
		NoZero, // afficher le zéro
		begin=true,beginlabel=true, // Affiche première graduation
		end=false,endlabel=false, // Affiche dernière graduation
		Step=yPasPrincipal,Size=.6mm,pTick=black, // Pas des graduations principales
		step=yPasSecondaire,size=.4mm,ptick=gray // Pas des graduations secondaires
	);
yaxis(scale(coef_labely+0.2)*Label(legende_y,position=EndPoint, align=2N+W), 
	ymin=ymin,ymax=ymax, 
	yticks,
	above=true,
	Arrow(6)); 
}

pair cpmax=max(currentpicture,true);
pair cpmin=min(currentpicture,true);
//write(cpmin,cpmax);
//xmax=currentpicture.userMax().x;
//ymax=currentpicture.userMax().y;
//write(xmax,ymax);

// Placement des points ////////////////////////////////////////////////
for (int k=0; k<coordonnees_points.length; ++k) {
	// si pas d'attribut pour un point, on prend ceux du premier point
	align alignement=(k>=alignements_points.length) ? alignements_points[0] : alignements_points[k];
	real diametre=(k>=diametres_points.length) ? diametres_points[0] : diametres_points[k];
	pen couleur=(k>=couleurs_points.length) ? couleurs_points[0] : couleurs_points[k];
	string legende=(k>=legendes_points.length) ? "" : legendes_points[k];
	pair coordonnees=coordonnees_points[k];
	// disque extérieur
	dot(coordonnees,diametre+couleur);
	// disque intérieur
	dot(coordonnees,couleur+0.7*white+(diametre-1.5bp));
	// légende
	label(scale(coef_label_points)*legende,coordonnees,alignement,couleur);
}

// Construction des courbes ////////////////////////////////////////////
picture picgraph;
for (int k=0; k<fonctions.length; ++k) {	
	if(fonctions[k].type=="car"){
		// Tracé de la courbe
		for (int j=0; j<fonctions[k].min.length; ++j) {
			draw(picgraph,graph(picgraph,fonctions[k].f,fonctions[k].min[j],fonctions[k].max[j],fonctions[k].n),fonctions[k].couleur_fonction+fonctions[k].epaisseur_fonction+fonctions[k].trait_fonction);
		}
		// Légende de la courbe
		label(scale(fonctions[k].coef_label_fct)*fonctions[k].legende,(fonctions[k].position_legende,fonctions[k].f(fonctions[k].position_legende)),fonctions[k].alignement_legende,fonctions[k].couleur_legende); 
		// Points
		for (int j=0; j<fonctions[k].parametres_points.length; ++j) {
			// si pas d'attribut pour un point, on prend ceux du premier point
			align alignement=(j>=fonctions[k].alignements_points.length) ? fonctions[k].alignements_points[0] : fonctions[k].alignements_points[j];
			real diametre=(j>=fonctions[k].diametres_points.length) ? fonctions[k].diametres_points[0] : fonctions[k].diametres_points[j];
			pen couleur=(j>=fonctions[k].couleurs_points.length) ? fonctions[k].couleurs_points[0] : fonctions[k].couleurs_points[j];			
			pair coordonnees=(fonctions[k].parametres_points[j],fonctions[k].f(fonctions[k].parametres_points[j]));
			// disque extérieur
			dot(coordonnees,diametre+fonctions[k].couleur_fonction);
			// disque intérieur
			dot(coordonnees,couleur+0.7*white+(diametre-1.5bp));
			// légende
			label(scale(fonctions[k].coef_label_fct)*fonctions[k].legendes_points[j],coordonnees,alignement,couleur);
		};
		// Tangentes
		for (int j=0; j<fonctions[k].parametres_tangentes.length; ++j) {
			// si pas d'attribut pour une tangente, on prend ceux de la première tangente
			pen couleur=(j>=fonctions[k].couleurs_tangentes.length) ? fonctions[k].couleurs_tangentes[0] : fonctions[k].couleurs_tangentes[j];
			real direction=(j>=fonctions[k].directions_tangentes.length) ? fonctions[k].directions_tangentes[0] : fonctions[k].directions_tangentes[j];
			real coefficient=(j>=fonctions[k].coefficient_tangentes.length) ? fonctions[k].coefficient_tangentes[0] : fonctions[k].coefficient_tangentes[j];
			pen epaisseur=(j>=fonctions[k].epaisseurs_tangentes.length) ? fonctions[k].epaisseurs_tangentes[0] : fonctions[k].epaisseurs_tangentes[j];
			pair coordonnees_a=(fonctions[k].parametres_tangentes[j],fonctions[k].f(fonctions[k].parametres_tangentes[j]));
			pair coordonnees_b=coordonnees_a+(1,nb_derive(fonctions[k].f,fonctions[k].parametres_tangentes[j]))*coefficient;
			if(direction==2){
				coordonnees_a=2*coordonnees_a-coordonnees_b;
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrows(HookHead,4bp));
			} else {
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrow(HookHead,4bp));
			}			
		}		
	} else if(fonctions[k].type=="pol"){
		// Tracé de la courbe
		for (int j=0; j<fonctions[k].min.length; ++j) {
			draw(picgraph,polargraph(picgraph,fonctions[k].rho,fonctions[k].min[j],fonctions[k].max[j],fonctions[k].n),fonctions[k].couleur_fonction+fonctions[k].epaisseur_fonction+fonctions[k].trait_fonction);
		}
		// Légende de la courbe
		label(scale(fonctions[k].coef_label_fct)*fonctions[k].legende,(fonctions[k].rho(fonctions[k].position_legende)*cos(fonctions[k].position_legende),fonctions[k].rho(fonctions[k].position_legende)*sin(fonctions[k].position_legende)),fonctions[k].alignement_legende,fonctions[k].couleur_legende); 
		// Points
		for (int j=0; j<fonctions[k].parametres_points.length; ++j) {
			// si pas d'attribut pour un point, on prend ceux du premier point
			align alignement=(j>=fonctions[k].alignements_points.length) ? fonctions[k].alignements_points[0] : fonctions[k].alignements_points[j];
			real diametre=(j>=fonctions[k].diametres_points.length) ? fonctions[k].diametres_points[0] : fonctions[k].diametres_points[j];
			pen couleur=(j>=fonctions[k].couleurs_points.length) ? fonctions[k].couleurs_points[0] : fonctions[k].couleurs_points[j];
			pair coordonnees=(fonctions[k].rho(fonctions[k].parametres_points[j])*cos(fonctions[k].parametres_points[j]),fonctions[k].rho(fonctions[k].parametres_points[j])*sin(fonctions[k].parametres_points[j]));
			// disque extérieur
			dot(coordonnees,diametre+fonctions[k].couleur_fonction);
			// disque intérieur
			dot(coordonnees,couleur+0.7*white+(diametre-1.5bp));
			// légende
			label(scale(fonctions[k].coef_label_fct)*fonctions[k].legendes_points[j],coordonnees,alignement,couleur);
		}
		// Tangentes
		for (int j=0; j<fonctions[k].parametres_tangentes.length; ++j) {
			// si pas d'attribut pour une tangente, on prend ceux de la première tangente
			pen couleur=(j>=fonctions[k].couleurs_tangentes.length) ? fonctions[k].couleurs_tangentes[0] : fonctions[k].couleurs_tangentes[j];
			real direction=(j>=fonctions[k].directions_tangentes.length) ? fonctions[k].directions_tangentes[0] : fonctions[k].directions_tangentes[j];
			real coefficient=(j>=fonctions[k].coefficient_tangentes.length) ? fonctions[k].coefficient_tangentes[0] : fonctions[k].coefficient_tangentes[j];
			pen epaisseur=(j>=fonctions[k].epaisseurs_tangentes.length) ? fonctions[k].epaisseurs_tangentes[0] : fonctions[k].epaisseurs_tangentes[j];
			pair coordonnees_a=(fonctions[k].rho(fonctions[k].parametres_tangentes[j])*cos(fonctions[k].parametres_tangentes[j]),fonctions[k].rho(fonctions[k].parametres_tangentes[j])*sin(fonctions[k].parametres_tangentes[j]));
			real theta=fonctions[k].parametres_tangentes[j];
			real rho_prime=nb_derive(fonctions[k].rho,theta);
			pair coordonnees_b=coordonnees_a+(rho_prime*cos(theta)-fonctions[k].rho(theta)*sin(theta),rho_prime*sin(theta)+fonctions[k].rho(theta)*cos(theta))*coefficient;
			if(direction==2){
				coordonnees_a=2*coordonnees_a-coordonnees_b;
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrows(HookHead,4bp));
			} else {
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrow(HookHead,4bp));
			}			
		}
	} else if(fonctions[k].type=="par"){
		// Tracé de la courbe
		for (int j=0; j<fonctions[k].min.length; ++j) {
			draw(picgraph,graph(picgraph,fonctions[k].x,fonctions[k].y,fonctions[k].min[j],fonctions[k].max[j],fonctions[k].n),fonctions[k].couleur_fonction+fonctions[k].epaisseur_fonction+fonctions[k].trait_fonction);
		}
		// Légende de la courbe
		label(scale(fonctions[k].coef_label_fct)*fonctions[k].legende,(fonctions[k].x(fonctions[k].position_legende),fonctions[k].y(fonctions[k].position_legende)),fonctions[k].alignement_legende,fonctions[k].couleur_legende);
		// Points
		for (int j=0; j<fonctions[k].parametres_points.length; ++j) {
			// si pas d'attribut pour un point, on prend ceux du premier point
			align alignement=(j>=fonctions[k].alignements_points.length) ? fonctions[k].alignements_points[0] : fonctions[k].alignements_points[j];
			real diametre=(j>=fonctions[k].diametres_points.length) ? fonctions[k].diametres_points[0] : fonctions[k].diametres_points[j];
			pen couleur=(j>=fonctions[k].couleurs_points.length) ? fonctions[k].couleurs_points[0] : fonctions[k].couleurs_points[j];
			pair coordonnees=(fonctions[k].x(fonctions[k].parametres_points[j]),fonctions[k].y(fonctions[k].parametres_points[j]));
			// disque extérieur
			dot(coordonnees,diametre+fonctions[k].couleur_fonction);
			// disque intérieur
			dot(coordonnees,couleur+0.7*white+(diametre-1.5bp));
			// légende
			label(scale(fonctions[k].coef_label_fct)*fonctions[k].legendes_points[j],coordonnees,alignement,couleur);
		}
		// Tangentes
		for (int j=0; j<fonctions[k].parametres_tangentes.length; ++j) {
			// si pas d'attribut pour une tangente, on prend ceux de la première tangente
			pen couleur=(j>=fonctions[k].couleurs_tangentes.length) ? fonctions[k].couleurs_tangentes[0] : fonctions[k].couleurs_tangentes[j];
			real direction=(j>=fonctions[k].directions_tangentes.length) ? fonctions[k].directions_tangentes[0] : fonctions[k].directions_tangentes[j];
			real coefficient=(j>=fonctions[k].coefficient_tangentes.length) ? fonctions[k].coefficient_tangentes[0] : fonctions[k].coefficient_tangentes[j];
			pen epaisseur=(j>=fonctions[k].epaisseurs_tangentes.length) ? fonctions[k].epaisseurs_tangentes[0] : fonctions[k].epaisseurs_tangentes[j];
			pair coordonnees_a=(fonctions[k].x(fonctions[k].parametres_tangentes[j]),fonctions[k].y(fonctions[k].parametres_tangentes[j]));
			pair coordonnees_b=coordonnees_a+(nb_derive(fonctions[k].x,fonctions[k].parametres_tangentes[j]),nb_derive(fonctions[k].y,fonctions[k].parametres_tangentes[j]))*coefficient;
			if(direction==2){
				coordonnees_a=2*coordonnees_a-coordonnees_b;
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrows(HookHead,4bp));
			} else {
				draw(coordonnees_a--coordonnees_b,couleur+epaisseur,Arrow(HookHead,4bp));
			}			
		}		
	}
  } 
 
// Limiter la fenêtre en hauteur
//ylimits(ymin,ymax,Crop);
//xlimits(xmin,xmax,Crop);
//fixedscaling(currentpicture, cpmin,cpmax);
//crop(currentpicture);
//limits((xmin,ymin),(xmax,ymax),Crop);
limits(picgraph,(xmin,ymin),(xmax,ymax),Crop);
add(picgraph);

if(repereoij) {
// Repère Oij
labelx(scale(coef_labelx)*Label("$O$",NoFill), 0, SW);
dot((0,0));
draw(scale(coef_labelx)*Label("$\vec{\imath}$",align=S,UnFill),
     (0,0)--(1,0),scale(1.2)*currentpen,Arrow(6));
draw(scale(coef_labely)*Label("$\vec{\jmath}$",align=W,UnFill),
     (0,0)--(0,1),scale(1.2)*currentpen,Arrow(6));
}


