
#include <chipmunk.h>

cpBody * inline_c_Apecs_Physics_Body_0_9de312df47e9008d73b49691c4a60dec6deeb631(cpSpace * space_inline_c_0, intptr_t ety_inline_c_1) {

    cpBody* body = cpBodyNew(0,0);
    cpSpaceAddBody(space_inline_c_0, body);
    cpBodySetUserData(body, (void*) ety_inline_c_1);
    return body; 
}


void inline_c_Apecs_Physics_Body_1_14bb13e049f9a11c73ac7739f14a66ae8b5b9cc6(cpBody * bodyPtr_inline_c_0, int bodyInt_inline_c_1) {
 cpBodySetType(bodyPtr_inline_c_0, bodyInt_inline_c_1) ;
}


int inline_c_Apecs_Physics_Body_2_32c32729e276ec7264805a38d2acc94dfedae018(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetType(bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_3_e3caadba03b1570aa4031126a0d115cb479e01cb(cpBody * bodyPtr_inline_c_0, cpSpace * space_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpSpaceRemoveBody(space_inline_c_1, body);
  cpBodyFree(body); 
}


double inline_c_Apecs_Physics_Body_4_01392e4e73e8f00d4c312ba7220e2bcb8cc948d2(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetPosition (bodyPtr_inline_c_0).x );
}


double inline_c_Apecs_Physics_Body_5_20231f564b446e3af856dbf883ae1130427a09dd(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetPosition (bodyPtr_inline_c_0).y );
}


void inline_c_Apecs_Physics_Body_6_2f6f07c7918e219dc512dd7ad57aa4d0033b07cb(double x_inline_c_0, double y_inline_c_1, cpBody * bodyPtr_inline_c_2) {

  const cpVect pos = { x_inline_c_0, y_inline_c_1 };
  cpBody *body = bodyPtr_inline_c_2;
  cpBodySetPosition(body, pos);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_7_87a8d89f29a8ce4c45f645af70c58f3a60550992(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetVelocity (bodyPtr_inline_c_0).x );
}


double inline_c_Apecs_Physics_Body_8_d7b8d30808b31a6bbdd6040eca023f05b52a4c63(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetVelocity (bodyPtr_inline_c_0).y );
}


void inline_c_Apecs_Physics_Body_9_bd432f162ec1d278eb72d92887d1b9f469b69038(double x_inline_c_0, double y_inline_c_1, cpBody * bodyPtr_inline_c_2) {

  const cpVect vel = { x_inline_c_0, y_inline_c_1 };
  cpBodySetVelocity(bodyPtr_inline_c_2, vel);
  
}


double inline_c_Apecs_Physics_Body_10_3be849cc8946a58cd962c66f7a3b5cce82aa8a58(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetAngle (bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_11_558c3ee22e2cd33be256c516ab87118900f06b47(cpBody * bodyPtr_inline_c_0, double angle_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpBodySetAngle(body, angle_inline_c_1);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_12_41c955970dc8afc7027b1b569d9b20e1914ea154(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetAngularVelocity (bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_13_aa0cdc4407352ef5ce48dd39449ff73a5f60b424(cpBody * bodyPtr_inline_c_0, double angle_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpBodySetAngularVelocity(body, angle_inline_c_1);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_14_949f78e1ba8da4aae24536d2293bddeedf7c1735(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetForce (bodyPtr_inline_c_0).x );
}


double inline_c_Apecs_Physics_Body_15_b0862382ac90c2745bb143b744c2bf63b31f347b(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetForce (bodyPtr_inline_c_0).y );
}


void inline_c_Apecs_Physics_Body_16_986c84564b4073aa925cc15fab46a8a23d4b0631(double x_inline_c_0, double y_inline_c_1, cpBody * bodyPtr_inline_c_2) {

  const cpVect frc = { x_inline_c_0, y_inline_c_1 };
  cpBodySetForce(bodyPtr_inline_c_2, frc);
  
}


double inline_c_Apecs_Physics_Body_17_e7f26e6a019bf65ca54218ca221a508e62beeade(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetMass (bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_18_e18516a70071526287601c73217308de9a0b8b5d(cpBody * bodyPtr_inline_c_0, double angle_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpBodySetMass(body, angle_inline_c_1);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_19_995c9a54ece727e0c4b102c3002f20810fed8320(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetMoment (bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_20_aecd5298cb6f6ba26edf2d2a5a91cdd0458609d9(cpBody * bodyPtr_inline_c_0, double angle_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpBodySetMoment(body, angle_inline_c_1);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_21_3c55d69b2648af9f4b8f854f982e2a94a6327ed9(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetTorque (bodyPtr_inline_c_0) );
}


void inline_c_Apecs_Physics_Body_22_339c2d1ce1085d4072e1010fd03dc60db3b3e38b(cpBody * bodyPtr_inline_c_0, double angle_inline_c_1) {

  cpBody *body = bodyPtr_inline_c_0;
  cpBodySetTorque(body, angle_inline_c_1);
  if (cpBodyGetType(body) == CP_BODY_TYPE_STATIC)
    cpSpaceReindexShapesForBody(cpBodyGetSpace(body), body);
  
}


double inline_c_Apecs_Physics_Body_23_a615e0e3bc8e7b2aca0ee61083e08fbc91c3b5bb(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetCenterOfGravity (bodyPtr_inline_c_0).x );
}


double inline_c_Apecs_Physics_Body_24_4e816a20006bb7a32a99f2c1884a141692785dd5(cpBody * bodyPtr_inline_c_0) {
return ( cpBodyGetCenterOfGravity (bodyPtr_inline_c_0).y );
}


void inline_c_Apecs_Physics_Body_25_77b2d0c63d56dccdfa682106ec4f41090bc9dfb3(double x_inline_c_0, double y_inline_c_1, cpBody * bodyPtr_inline_c_2) {

  const cpVect vel = { x_inline_c_0, y_inline_c_1 };
  cpBodySetCenterOfGravity(bodyPtr_inline_c_2, vel);
  
}

