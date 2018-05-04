
#include <chipmunk.h>

cpShape * inline_c_Apecs_Physics_Shape_0_05cac00fae8b6041a16d6896d802891bc2354e34(double x_inline_c_0, double y_inline_c_1, cpBody * bodyPtr_inline_c_2, double radius_inline_c_3, intptr_t ety_inline_c_4, cpSpace * spacePtr_inline_c_5) {

      const cpVect vec = { x_inline_c_0, y_inline_c_1 };
      cpShape* sh = cpCircleShapeNew(bodyPtr_inline_c_2, radius_inline_c_3, vec);
      cpShapeSetUserData(sh, (void*) ety_inline_c_4);
      return cpSpaceAddShape( spacePtr_inline_c_5, sh); 
}


cpShape * inline_c_Apecs_Physics_Shape_1_b1c05c0446a19796289b3cc6c3f55173f3768838(double xa_inline_c_0, double ya_inline_c_1, double xb_inline_c_2, double yb_inline_c_3, cpBody * bodyPtr_inline_c_4, double radius_inline_c_5, intptr_t ety_inline_c_6, cpSpace * spacePtr_inline_c_7) {

       const cpVect va = { xa_inline_c_0, ya_inline_c_1 };
       const cpVect vb = { xb_inline_c_2, yb_inline_c_3 };
       cpShape* sh = cpSegmentShapeNew(bodyPtr_inline_c_4, va, vb, radius_inline_c_5);
       cpShapeSetUserData(sh, (void*) ety_inline_c_6);
       return cpSpaceAddShape( spacePtr_inline_c_7, sh); 
}


cpShape * inline_c_Apecs_Physics_Shape_2_5b43203b55b0d1f5bc6d5caffa798a44a851746c(cpBody * bodyPtr_inline_c_0, long vec_inline_c_1, cpVect * vec_inline_c_2, double radius_inline_c_3, intptr_t ety_inline_c_4, cpSpace * spacePtr_inline_c_5) {

           cpTransform trans = cpTransformIdentity;
           cpShape* sh = cpPolyShapeNew(bodyPtr_inline_c_0, vec_inline_c_1, vec_inline_c_2, trans, radius_inline_c_3);
           cpShapeSetUserData(sh, (void*) ety_inline_c_4);
           return cpSpaceAddShape( spacePtr_inline_c_5, sh); 
}


void inline_c_Apecs_Physics_Shape_3_e2c719ac5e0e46fe9584a11bae24cb08fdc84cf0(cpShape * shapePtr_inline_c_0, cpSpace * space_inline_c_1) {

  cpShape *shape = shapePtr_inline_c_0;
  cpSpaceRemoveShape(space_inline_c_1, shape);
  cpShapeFree (shape); 
}


int inline_c_Apecs_Physics_Shape_4_f1823d0c9a6dbdf80d98aefa9a73983205112750(cpShape * shape_inline_c_0) {
return (
  cpShapeGetSensor(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_5_7488000dfb73c9fc4aee2e0cdb4ae61fa97f3220(cpShape * shape_inline_c_0, int isSensor_inline_c_1) {

  cpShapeSetSensor(shape_inline_c_0, isSensor_inline_c_1) ;
}


double inline_c_Apecs_Physics_Shape_6_0d6c7e597e1ca75c18fc7ee3476b9779bd9bbb2d(cpShape * shape_inline_c_0) {
return (
  cpShapeGetElasticity(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_7_0355aa3c361665713c247af3863a884e1d787dab(cpShape * shape_inline_c_0, double elasticity_inline_c_1) {

  cpShapeSetElasticity(shape_inline_c_0, elasticity_inline_c_1) ;
}


double inline_c_Apecs_Physics_Shape_8_01fb5e3eab1bfcb6e536962f15d01b92486ef800(cpShape * shape_inline_c_0) {
return (
  cpShapeGetMass(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_9_eb0fa14fe75f2dfded1106bfd197208793c3478f(cpShape * shape_inline_c_0, double mass_inline_c_1) {

  cpShapeSetMass(shape_inline_c_0, mass_inline_c_1) ;
}


double inline_c_Apecs_Physics_Shape_10_9dcf416b0464589cb673fed70c220020538b3baf(cpShape * shape_inline_c_0) {
return (
  cpShapeGetDensity(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_11_900ceaff53a04b6507e5548c1a9b43879c71f506(cpShape * shape_inline_c_0, double density_inline_c_1) {

  cpShapeSetDensity(shape_inline_c_0, density_inline_c_1) ;
}


double inline_c_Apecs_Physics_Shape_12_025e9bed3b915c5eb3ddb1c4cc68a8d96eadcfd3(cpShape * shape_inline_c_0) {
return (
  cpShapeGetFriction(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_13_7f29d921ef4105a642bd83104211e814e859bb43(cpShape * shape_inline_c_0, double friction_inline_c_1) {

  cpShapeSetFriction(shape_inline_c_0, friction_inline_c_1) ;
}


double inline_c_Apecs_Physics_Shape_14_0760a7c3db583f2a823cf93e0586ff4043196b3c(cpShape * shape_inline_c_0) {
return ( cpShapeGetSurfaceVelocity(shape_inline_c_0).x );
}


double inline_c_Apecs_Physics_Shape_15_4a185a064f9ba8b1143176444dfe9769a9618fdb(cpShape * shape_inline_c_0) {
return ( cpShapeGetSurfaceVelocity(shape_inline_c_0).y );
}


void inline_c_Apecs_Physics_Shape_16_b6f13e21078f0d7587f8d223c3c9815d2b43a54b(double x_inline_c_0, double y_inline_c_1, cpShape * shape_inline_c_2) {

  const cpVect vec = { x_inline_c_0, y_inline_c_1 };
  cpShapeSetSurfaceVelocity(shape_inline_c_2, vec);
  
}


unsigned inline_c_Apecs_Physics_Shape_17_5b8ac07da5cd4c58105bc182d0f2bdd6f0a35db2(cpShape * shape_inline_c_0) {
return ( cpShapeGetFilter(shape_inline_c_0).group );
}


unsigned inline_c_Apecs_Physics_Shape_18_dc1760971843013d2572142c3f8e8b1c074f18b8(cpShape * shape_inline_c_0) {
return ( cpShapeGetFilter(shape_inline_c_0).categories );
}


unsigned inline_c_Apecs_Physics_Shape_19_d6f830f6e22e8263b1b314b8ce416d34bddbd95a(cpShape * shape_inline_c_0) {
return ( cpShapeGetFilter(shape_inline_c_0).mask );
}


void inline_c_Apecs_Physics_Shape_20_8c9cbbfb84dd49b3433daeecd543bd518c55a58c(unsigned group_inline_c_0, unsigned cats_inline_c_1, unsigned mask_inline_c_2, cpShape * shape_inline_c_3) {

    const cpShapeFilter filter = { group_inline_c_0
                                 , cats_inline_c_1
                                 , mask_inline_c_2 };
    cpShapeSetFilter(shape_inline_c_3, filter);
  
}


uintptr_t inline_c_Apecs_Physics_Shape_21_cfe0b2c724fcefabc23edaa850608e1110e5b651(cpShape * shape_inline_c_0) {
return (
  cpShapeGetCollisionType(shape_inline_c_0) );
}


void inline_c_Apecs_Physics_Shape_22_d46ae5988d50482952b3fed819ea5f2890b73755(cpShape * shape_inline_c_0, uintptr_t ctype_inline_c_1) {

  cpShapeSetCollisionType(shape_inline_c_0, ctype_inline_c_1) ;
}


uintptr_t inline_c_Apecs_Physics_Shape_23_abb6c3a8a96f6b87e5c7656f7f49efb66aea3cf8(cpShape * shape_inline_c_0) {
return (
  (intptr_t) cpBodyGetUserData(cpShapeGetBody(shape_inline_c_0)) );
}

