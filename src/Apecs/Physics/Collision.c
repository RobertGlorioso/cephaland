
#include <chipmunk.h>

#include <chipmunk_structs.h>

double inline_c_Apecs_Physics_Collision_0_cbc7c0ad44bad0ca39f504f0e1b6186b0f9f5530(cpArbiter * arb_inline_c_0) {
return ( cpArbiterGetNormal(arb_inline_c_0).x );
}


double inline_c_Apecs_Physics_Collision_1_5a7c72c3e266b399e1ea5d1bbc11fb329a7c32e5(cpArbiter * arb_inline_c_0) {
return ( cpArbiterGetNormal(arb_inline_c_0).y );
}


unsigned inline_c_Apecs_Physics_Collision_2_4ebb8d58cf297b3607583dceb8e9ddc9211f3c52(cpArbiter * arb_inline_c_0) {
 CP_ARBITER_GET_BODIES(arb_inline_c_0, ba, bb); return (intptr_t) (ba->userData); 
}


unsigned inline_c_Apecs_Physics_Collision_3_aa85f4323f9dd767d55a2b4e12aa425f30c54218(cpArbiter * arb_inline_c_0) {
 CP_ARBITER_GET_BODIES(arb_inline_c_0, ba, bb); return (intptr_t) (bb->userData); 
}


double inline_c_Apecs_Physics_Collision_4_cbc7c0ad44bad0ca39f504f0e1b6186b0f9f5530(cpArbiter * arb_inline_c_0) {
return ( cpArbiterGetNormal(arb_inline_c_0).x );
}


double inline_c_Apecs_Physics_Collision_5_5a7c72c3e266b399e1ea5d1bbc11fb329a7c32e5(cpArbiter * arb_inline_c_0) {
return ( cpArbiterGetNormal(arb_inline_c_0).y );
}


unsigned inline_c_Apecs_Physics_Collision_6_4ebb8d58cf297b3607583dceb8e9ddc9211f3c52(cpArbiter * arb_inline_c_0) {
 CP_ARBITER_GET_BODIES(arb_inline_c_0, ba, bb); return (intptr_t) (ba->userData); 
}


unsigned inline_c_Apecs_Physics_Collision_7_aa85f4323f9dd767d55a2b4e12aa425f30c54218(cpArbiter * arb_inline_c_0) {
 CP_ARBITER_GET_BODIES(arb_inline_c_0, ba, bb); return (intptr_t) (bb->userData); 
}


cpCollisionHandler * inline_c_Apecs_Physics_Collision_8_0f8bc12d6ce810ccbe614d7b33bc2ff4e6bc3683(cpSpace * space_inline_c_0, unsigned cta_inline_c_1, unsigned ctb_inline_c_2) {
return (cpSpaceAddCollisionHandler(space_inline_c_0, cta_inline_c_1, ctb_inline_c_2));
}


cpCollisionHandler * inline_c_Apecs_Physics_Collision_9_6d99d15a1ca79de8d84ceabfaaf42f0b01959fd2(cpSpace * space_inline_c_0, unsigned ct_inline_c_1) {
return (cpSpaceAddWildcardHandler(space_inline_c_0, ct_inline_c_1));
}


void inline_c_Apecs_Physics_Collision_10_73580a6bc352215747b8d18f203828dc1965e674(cpCollisionHandler * handler_inline_c_0, intptr_t ety_inline_c_1) {
 handler_inline_c_0->userData = (void*) ety_inline_c_1 ;
}


void inline_c_Apecs_Physics_Collision_11_bf326a866aaded92d6efc550973867cff14865d7(cpCollisionHandler * handler_inline_c_0, void * fn_inline_c_1) {
 handler_inline_c_0->beginFunc = fn_inline_c_1 ;
}


void inline_c_Apecs_Physics_Collision_12_c2d678bec6cfab54190b946e5b10a3674dbfe185(cpCollisionHandler * handler_inline_c_0, void * fn_inline_c_1) {
 handler_inline_c_0->separateFunc = fn_inline_c_1 ;
}


void inline_c_Apecs_Physics_Collision_13_b3307dbc57c846d2f1ffc38c4d99e1aa89c2375b(cpCollisionHandler * handler_inline_c_0, void * fn_inline_c_1) {
 handler_inline_c_0->preSolveFunc = fn_inline_c_1 ;
}


void inline_c_Apecs_Physics_Collision_14_ef24d4f23bbf2b93e522c4a614c5b2314284fa71(cpCollisionHandler * handler_inline_c_0, void * fn_inline_c_1) {
 handler_inline_c_0->postSolveFunc = fn_inline_c_1 ;
}

