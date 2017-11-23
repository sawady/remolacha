#include <iostream>
#include <stdlib.h>

using namespace std;

typedef unsigned long long int Num;
typedef const char* String;
typedef void* PTR;

struct Clase {
  string nombre;
  PTR* metodos;
};

struct Objeto {
  Clase* clase;
  PTR* varsInstancia;
};

typedef Objeto* (*Metodo)(...);

#define NUM_TO_PTR(N) ((PTR)(N)) /* Convierte Num -> PTR */
#define PTR_TO_NUM(P) ((Num)(P)) /* Convierte PTR -> Num */
#define STRING_TO_PTR(S) ((PTR)(S)) /* Convierte String -> PTR */
#define PTR_TO_STRING(P) ((String)(P)) /* Convierte PTR -> String */
#define METHOD_TO_PTR(M) ((PTR)(M)) /* Convierte Metodo -> PTR */
#define PTR_TO_METHOD(P) ((Metodo)(P)) /* Convierte PTR -> Metodo */
#define OBJECT_TO_PTR(O) ((PTR)(O)) /* Convierte Objeto* -> PTR */
#define PTR_TO_OBJECT(P) ((Objeto*)(P)) /* Convierte PTR -> Objeto* */

Clase* cls0; /* Int */
Clase* cls1; /* String */
Clase* cls2; /* Contador */
Clase* cls3; /* Main */

/* Construye un objeto de clase Int */
Objeto* constructor_cls0(Num valor) {
  Objeto* obj = new Objeto;
  obj->clase = cls0; /* Int */
  obj->varsInstancia = new PTR[1];
  obj->varsInstancia[0] = NUM_TO_PTR(valor);
  return obj;
}

/* Construye un objeto de clase String */
Objeto* constructor_cls1(String valor) {
  Objeto* obj = new Objeto;
  obj->clase = cls1; /* String */
  obj->varsInstancia = new PTR[1];
  obj->varsInstancia[0] = STRING_TO_PTR(valor);
  return obj;
}

#define DEFAULT_VALUE constructor_cls0(0)

#define GET_LOCAL(i) (PTR_TO_OBJECT(o0->varsInstancia[i]))

Objeto* ASSIGN_LOCAL(Objeto* o0, int i, Objeto* x) {
  o0->varsInstancia[i] = OBJECT_TO_PTR(x);
  return DEFAULT_VALUE;
}

Objeto* ASSIGN_PARAM(Objeto*& param, Objeto* x) {
  param = x;
  return DEFAULT_VALUE;
}

Objeto* CALL(Objeto* r, string m, int i, Objeto* args...) {
  if(r->clase->metodos[i] == NULL) {
    cout << r->clase->nombre << " no acepta el mensaje " << m << endl;
    exit(-1);
  }
  return PTR_TO_METHOD(r->clase->metodos[i])(r, args);
}

/* Construye un objeto de la clase Contador */
Objeto* constructor_cls2() {
  Objeto* obj = new Objeto;
  obj->clase = cls2; /* Contador */
  obj->varsInstancia = new PTR[1];
  obj->varsInstancia[0] = OBJECT_TO_PTR(DEFAULT_VALUE);
  return obj;
}

/* Construye un objeto de la clase Main */
Objeto* constructor_cls3() {
  Objeto* obj = new Objeto;
  obj->clase = cls3; /* Main */
  obj->varsInstancia = new PTR[1];
  obj->varsInstancia[0] = OBJECT_TO_PTR(DEFAULT_VALUE);
  return obj;
}


/* cls0 => Int , sel0 => print/0 */
Objeto* met_cls0_sel0(Objeto* o0) {
  cout << PTR_TO_NUM(o0->varsInstancia[0]) << endl;
  return o0;
}

/* cls0 => Int , sel1 => add/1 */
Objeto* met_cls0_sel1(Objeto* o0, Objeto* o1) {
  Num n1 = PTR_TO_NUM(o0->varsInstancia[0]);
  Num n2 = PTR_TO_NUM(o1->varsInstancia[0]);
  return constructor_cls0(n1 + n2);
}

/* cls0 => String , sel0 => print/0 */
Objeto* met_cls1_sel0(Objeto* o0) {
  cout << PTR_TO_STRING(o0->varsInstancia[0]) << endl;
  return o0;
}

/* cls1 => String , sel1 => add/1 */
Objeto* met_cls1_sel1(Objeto* o0, Objeto* o1) {
  String s1 = PTR_TO_STRING(o0->varsInstancia[0]);
  String s2 = PTR_TO_STRING(o1->varsInstancia[0]);
  string str1(s1);
  string str2(s2);
  const char* result = (str1 + str2).c_str();
  return constructor_cls1(result);
}

/* cls2 => Contador , sel4 => inicializar/1 */
Objeto* met_cls2_sel4(Objeto* o0, Objeto* o1) {
  ASSIGN_LOCAL(o0, 0, o1);
  return o0;
}

/* cls2 => Contador , sel2 => incrementar/0 */
Objeto* met_cls2_sel2(Objeto* o0) {
  return CALL(o0, "incrementarEn/1", 3, constructor_cls0(1));
}

/* cls2 => Contador , sel3 => incrementarEn/1 */
Objeto* met_cls2_sel3(Objeto* o0, Objeto* o1) {
  ASSIGN_LOCAL(o0, 0, CALL(GET_LOCAL(0), "add/1", 1, o1));
  return o0;
}

/* cls2 => Contador , sel6 => valorActual/0 */
Objeto* met_cls2_sel6(Objeto* o0) {
  return GET_LOCAL(0);
}

/* cls3 => Main , sel5 => main/0 */
Objeto* met_cls3_sel5(Objeto* o0) {
  ASSIGN_LOCAL(o0, 0, CALL(constructor_cls2(), "inicializar/1", 4, constructor_cls0(0)));
  CALL(CALL(CALL(GET_LOCAL(0), "incrementar/0", 2, NULL), "valorActual/0", 6, NULL), "print/0", 0, NULL);
  CALL(CALL(CALL(GET_LOCAL(0), "incrementar/0", 2, NULL), "valorActual/0", 6, NULL), "print/0", 0, NULL);
  return CALL(CALL(CALL(GET_LOCAL(0), "incrementarEn/1", 3, constructor_cls0(10)), "valorActual/0", 6, NULL), "print/0", 0, NULL);
}


int main() {

  /* Inicialización de la clase cls0 ( Int ) */
  cls0 = new Clase;
  cls0->nombre = "Int";
  cls0->metodos = new PTR[7];
  cls0->metodos[0] = METHOD_TO_PTR(met_cls0_sel0); /* print/0 */
  cls0->metodos[1] = METHOD_TO_PTR(met_cls0_sel1); /* add/1 */
  cls0->metodos[2] = NULL; /* incrementar/0 */
  cls0->metodos[3] = NULL; /* incrementarEn/1 */
  cls0->metodos[4] = NULL; /* inicializar/1 */
  cls0->metodos[5] = NULL; /* main/0 */
  cls0->metodos[6] = NULL; /* valorActual/0 */

  /* Inicialización de la clase cls1 ( String ) */
  cls1 = new Clase;
  cls1->nombre = "String";
  cls1->metodos = new PTR[7];
  cls1->metodos[0] = METHOD_TO_PTR(met_cls1_sel0); /* print/0 */
  cls1->metodos[1] = METHOD_TO_PTR(met_cls1_sel1); /* add/1 */
  cls1->metodos[2] = NULL; /* incrementar/0 */
  cls1->metodos[3] = NULL; /* incrementarEn/1 */
  cls1->metodos[4] = NULL; /* inicializar/1 */
  cls1->metodos[5] = NULL; /* main/0 */
  cls1->metodos[6] = NULL; /* valorActual/0 */

  /* Inicialización de la clase cls2 ( Contador ) */
  cls2 = new Clase;
  cls2->nombre = "Contador";
  cls2->metodos = new PTR[7];
  cls2->metodos[0] = NULL; /* print/0 */
  cls2->metodos[1] = NULL; /* add/1 */
  cls2->metodos[2] = METHOD_TO_PTR(met_cls2_sel2); /* incrementar/0 */
  cls2->metodos[3] = METHOD_TO_PTR(met_cls2_sel3); /* incrementarEn/1 */
  cls2->metodos[4] = METHOD_TO_PTR(met_cls2_sel4); /* inicializar/1 */
  cls2->metodos[5] = NULL; /* main/0 */
  cls2->metodos[6] = METHOD_TO_PTR(met_cls2_sel6); /* valorActual/0 */

  /* Inicialización de la clase cls3 ( Main ) */
  cls3 = new Clase;
  cls3->nombre = "Main";
  cls3->metodos = new PTR[7];
  cls3->metodos[0] = NULL; /* print/0 */
  cls3->metodos[1] = NULL; /* add/1 */
  cls3->metodos[2] = NULL; /* incrementar/0 */
  cls3->metodos[3] = NULL; /* incrementarEn/1 */
  cls3->metodos[4] = NULL; /* inicializar/1 */
  cls3->metodos[5] = METHOD_TO_PTR(met_cls3_sel5); /* main/0 */
  cls3->metodos[6] = NULL; /* valorActual/0 */

  /* Ejecución del programa principal */
  CALL(constructor_cls3(), "main/0", 5, NULL);

  return 0;
}
