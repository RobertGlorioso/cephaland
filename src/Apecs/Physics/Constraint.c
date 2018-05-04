
#include <chipmunk.h>

cpConstraint * inline_c_Apecs_Physics_Constraint_0_bc280de3c923907aeec996fa8cbfaddbac61e1f7(double ax_inline_c_0, double ay_inline_c_1, double bx_inline_c_2, double by_inline_c_3, cpBody * bodyA_inline_c_4, cpBody * bodyB_inline_c_5, intptr_t ety_inline_c_6, cpSpace * space_inline_c_7) {

    cpVect anchorA = cpv( ax_inline_c_0, ay_inline_c_1 );
    cpVect anchorB = cpv( bx_inline_c_2, by_inline_c_3 );
    cpConstraint* constraint = cpPinJointNew(bodyA_inline_c_4, bodyB_inline_c_5,anchorA,anchorB);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_6);
    return cpSpaceAddConstraint(space_inline_c_7, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_1_f6d52a3bb0774adb7007033bb513a648d89e202b(double ax_inline_c_0, double ay_inline_c_1, double bx_inline_c_2, double by_inline_c_3, cpBody * bodyA_inline_c_4, cpBody * bodyB_inline_c_5, double min_inline_c_6, double max_inline_c_7, intptr_t ety_inline_c_8, cpSpace * space_inline_c_9) {

    cpVect anchorA = cpv( ax_inline_c_0, ay_inline_c_1 );
    cpVect anchorB = cpv( bx_inline_c_2, by_inline_c_3 );
    cpConstraint* constraint = cpSlideJointNew(bodyA_inline_c_4, bodyB_inline_c_5,anchorA,anchorB,min_inline_c_6,max_inline_c_7);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_8);
    return cpSpaceAddConstraint(space_inline_c_9, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_2_8338dc1801229d9f050654dc6eb9e6729d56caca(double x_inline_c_0, double y_inline_c_1, cpBody * bodyA_inline_c_2, cpBody * bodyB_inline_c_3, intptr_t ety_inline_c_4, cpSpace * space_inline_c_5) {

    cpVect anchor = cpv( x_inline_c_0, y_inline_c_1 );
    cpConstraint* constraint = cpPivotJointNew(bodyA_inline_c_2, bodyB_inline_c_3, anchor);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_4);
    return cpSpaceAddConstraint(space_inline_c_5, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_3_aa0407904922b66759d5f7c1470b43bbc9516894(double ax_inline_c_0, double ay_inline_c_1, double bx_inline_c_2, double by_inline_c_3, cpBody * bodyA_inline_c_4, cpBody * bodyB_inline_c_5, intptr_t ety_inline_c_6, cpSpace * space_inline_c_7) {

    cpVect va = cpv( ax_inline_c_0, ay_inline_c_1 );
    cpVect vb = cpv( bx_inline_c_2, by_inline_c_3 );
    cpConstraint* constraint = cpPivotJointNew2(bodyA_inline_c_4, bodyB_inline_c_5, va, vb);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_6);
    return cpSpaceAddConstraint(space_inline_c_7, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_4_9e279441113da4e71d4a7ab2ee27f62f84c7f750(double ax_inline_c_0, double ay_inline_c_1, double bx_inline_c_2, double by_inline_c_3, double ancx_inline_c_4, double ancy_inline_c_5, cpBody * bodyA_inline_c_6, cpBody * bodyB_inline_c_7, intptr_t ety_inline_c_8, cpSpace * space_inline_c_9) {

    cpVect va = cpv( ax_inline_c_0, ay_inline_c_1 );
    cpVect vb = cpv( bx_inline_c_2, by_inline_c_3 );
    cpVect anchor = cpv( ancx_inline_c_4, ancy_inline_c_5 );
    cpConstraint* constraint = cpGrooveJointNew(bodyA_inline_c_6, bodyB_inline_c_7, va, vb, anchor);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_8);
    return cpSpaceAddConstraint(space_inline_c_9, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_5_982930b3e52a44280b25d661cba94e97e79f9812(double ax_inline_c_0, double ay_inline_c_1, double bx_inline_c_2, double by_inline_c_3, cpBody * bodyA_inline_c_4, cpBody * bodyB_inline_c_5, double rl_inline_c_6, double stf_inline_c_7, double damping_inline_c_8, intptr_t ety_inline_c_9, cpSpace * space_inline_c_10) {

    cpVect va = cpv( ax_inline_c_0, ay_inline_c_1 );
    cpVect vb = cpv( bx_inline_c_2, by_inline_c_3 );
    cpConstraint* constraint = cpDampedSpringNew(bodyA_inline_c_4, bodyB_inline_c_5, va, vb, rl_inline_c_6, stf_inline_c_7, damping_inline_c_8);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_9);
    return cpSpaceAddConstraint(space_inline_c_10, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_6_9da3893ce2e12f0b299c286579c0b0b53b4add35(cpBody * bodyA_inline_c_0, cpBody * bodyB_inline_c_1, double ra_inline_c_2, double stf_inline_c_3, double damping_inline_c_4, intptr_t ety_inline_c_5, cpSpace * space_inline_c_6) {

    cpConstraint* constraint = cpDampedRotarySpringNew(bodyA_inline_c_0, bodyB_inline_c_1, ra_inline_c_2, stf_inline_c_3, damping_inline_c_4);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_5);
    return cpSpaceAddConstraint(space_inline_c_6, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_7_f86611ab9f414a48a4086587eba9112bc2ce7742(cpBody * bodyA_inline_c_0, cpBody * bodyB_inline_c_1, double min_inline_c_2, double max_inline_c_3, intptr_t ety_inline_c_4, cpSpace * space_inline_c_5) {

    cpConstraint* constraint = cpRotaryLimitJointNew(bodyA_inline_c_0, bodyB_inline_c_1, min_inline_c_2, max_inline_c_3);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_4);
    return cpSpaceAddConstraint(space_inline_c_5, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_8_9ed4bed88fc671c41eebbb838c2f5387934322a6(cpBody * bodyA_inline_c_0, cpBody * bodyB_inline_c_1, double phase_inline_c_2, double ratchet_inline_c_3, intptr_t ety_inline_c_4, cpSpace * space_inline_c_5) {

    cpConstraint* constraint = cpRatchetJointNew(bodyA_inline_c_0, bodyB_inline_c_1, phase_inline_c_2, ratchet_inline_c_3);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_4);
    return cpSpaceAddConstraint(space_inline_c_5, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_9_40fa9e59078d56ed550e9d6fa83a7d12aa82efc3(cpBody * bodyA_inline_c_0, cpBody * bodyB_inline_c_1, double phase_inline_c_2, double ratio_inline_c_3, intptr_t ety_inline_c_4, cpSpace * space_inline_c_5) {

    cpConstraint* constraint = cpGearJointNew(bodyA_inline_c_0, bodyB_inline_c_1, phase_inline_c_2, ratio_inline_c_3);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_4);
    return cpSpaceAddConstraint(space_inline_c_5, constraint);
    
}


cpConstraint * inline_c_Apecs_Physics_Constraint_10_3126ee9dfda4aa50d7b3f609b70e06c0f77d4349(cpBody * bodyA_inline_c_0, cpBody * bodyB_inline_c_1, double rate_inline_c_2, intptr_t ety_inline_c_3, cpSpace * space_inline_c_4) {

    cpConstraint* constraint = cpSimpleMotorNew(bodyA_inline_c_0, bodyB_inline_c_1, rate_inline_c_2);
    cpConstraintSetUserData(constraint, (void*) ety_inline_c_3);
    return cpSpaceAddConstraint(space_inline_c_4, constraint);
    
}


void inline_c_Apecs_Physics_Constraint_11_b4fa1b9827a86fa2c4476217f64ca087447a0ea9(cpConstraint * constraintPtr_inline_c_0, cpSpace * space_inline_c_1) {

  cpConstraint *constraint = constraintPtr_inline_c_0;
  cpSpaceRemoveConstraint(space_inline_c_1, constraint);
  cpConstraintFree(constraint); 
}


intptr_t inline_c_Apecs_Physics_Constraint_12_9d656274fae098c4001e780b25fb630303c96a7b(cpConstraint * c_inline_c_0) {
return (
  (intptr_t) cpBodyGetUserData(cpConstraintGetBodyA(c_inline_c_0)) );
}


intptr_t inline_c_Apecs_Physics_Constraint_13_737f37b15cd704230c1795a125e5a068b4d803fe(cpConstraint * c_inline_c_0) {
return (
  (intptr_t) cpBodyGetUserData(cpConstraintGetBodyB(c_inline_c_0)) );
}


double inline_c_Apecs_Physics_Constraint_14_df30dd4f456bb4340f3d8515371357b81404c286(cpConstraint * c_inline_c_0) {
return ( cpConstraintGetMaxForce (c_inline_c_0) );
}


void inline_c_Apecs_Physics_Constraint_15_819d51c8ae39c59381f6be7a5f4f68a219ead813(cpConstraint * c_inline_c_0, double maxForce_inline_c_1) {
 cpConstraintSetMaxForce(c_inline_c_0, maxForce_inline_c_1); ;
}


double inline_c_Apecs_Physics_Constraint_16_f832acd9259f1feb677bd6c34b5f8b2d897212f2(cpConstraint * c_inline_c_0) {
return ( cpConstraintGetMaxBias (c_inline_c_0) );
}


void inline_c_Apecs_Physics_Constraint_17_327e3e45c120e5633a918c51a405b342c03f3f0f(cpConstraint * c_inline_c_0, double maxBias_inline_c_1) {
 cpConstraintSetMaxBias(c_inline_c_0, maxBias_inline_c_1); ;
}


double inline_c_Apecs_Physics_Constraint_18_c6f42365103d9d93aeb15c17f3e77a740666f8c2(cpConstraint * c_inline_c_0) {
return ( cpConstraintGetErrorBias (c_inline_c_0) );
}


void inline_c_Apecs_Physics_Constraint_19_c5a2144d4c223d68c027dfaee8052809b4e3aafe(cpConstraint * c_inline_c_0, double errorBias_inline_c_1) {
 cpConstraintSetErrorBias(c_inline_c_0, errorBias_inline_c_1); ;
}


int inline_c_Apecs_Physics_Constraint_20_331b8e8dfd25624a688ef4704095c39d354771a0(cpConstraint * c_inline_c_0) {
return ( cpConstraintGetCollideBodies (c_inline_c_0) );
}


void inline_c_Apecs_Physics_Constraint_21_e1f3ca62e20e1d905bb946e6fb0d1a6da0466995(cpConstraint * c_inline_c_0, int collide_inline_c_1) {
 cpConstraintSetCollideBodies(c_inline_c_0, collide_inline_c_1); ;
}

