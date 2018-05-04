
#include <chipmunk.h>

cpSpace * inline_c_Apecs_Physics_Space_0_6ffb6cb94175a5e9b5257043e660f5e98ddd253c() {
return ( cpSpaceNew() );
}


void inline_c_Apecs_Physics_Space_1_72cedf8e2f5e84df4b7ff6b3997bfa8d9b5cd533(cpSpace * spaceRaw_inline_c_0) {
 cpSpaceFree(spaceRaw_inline_c_0) ;
}


void inline_c_Apecs_Physics_Space_2_eacafef8d953c8b6c55056b5cec23c0e10cff641(cpSpace * space_inline_c_0, double dT_inline_c_1) {
 cpSpaceStep( space_inline_c_0, dT_inline_c_1 ) ;
}


double inline_c_Apecs_Physics_Space_3_3db871fd330077f8085373f004fbb5986f3bcb21(cpSpace * space_inline_c_0) {
return ( cpSpaceGetGravity (space_inline_c_0).x );
}


double inline_c_Apecs_Physics_Space_4_cf4bc0d5342c145cf15e8240a4e375f9a5ec6abe(cpSpace * space_inline_c_0) {
return ( cpSpaceGetGravity (space_inline_c_0).y );
}


void inline_c_Apecs_Physics_Space_5_6db2ce709f1e8cf43c6ac4a1e1256cc42909bb85(double x_inline_c_0, double y_inline_c_1, cpSpace * space_inline_c_2) {

  const cpVect vec = { x_inline_c_0, y_inline_c_1 };
  cpSpaceSetGravity(space_inline_c_2, vec);
  
}


int inline_c_Apecs_Physics_Space_6_8d9fd9cb1939778a15eb4580a2ff3061e9340e79(cpSpace * space_inline_c_0) {
return ( cpSpaceGetIterations (space_inline_c_0) );
}


void inline_c_Apecs_Physics_Space_7_5499a403d87ef2e46ddd3f0a4fbb0e97d6816da0(cpSpace * space_inline_c_0, int its_inline_c_1) {

  cpSpaceSetIterations(space_inline_c_0, its_inline_c_1);
  
}

