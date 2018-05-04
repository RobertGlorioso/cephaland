
#include <chipmunk.h>

void inline_c_Apecs_Physics_Query_0_348e116baa278a4b4abbfb2cb456ecc96a48d6ee(cpPointQueryInfo * pq_inline_c_0, cpSpace * space_inline_c_1, double px_inline_c_2, double py_inline_c_3, double maxDistance_inline_c_4, unsigned gr_inline_c_5, unsigned cs_inline_c_6, unsigned mk_inline_c_7) {

      cpPointQueryInfo *pq = pq_inline_c_0;
      cpSpacePointQueryNearest
        ( space_inline_c_1
        , cpv(px_inline_c_2, py_inline_c_3)
        , maxDistance_inline_c_4
        , cpShapeFilterNew(gr_inline_c_5, cs_inline_c_6, mk_inline_c_7)
        , pq);
      
}


intptr_t inline_c_Apecs_Physics_Query_1_2885daed5f20645e17a5ddb6f3c14e9affc55130(cpShape * sPtr_inline_c_0) {

            cpShape *shape = sPtr_inline_c_0;
            if (shape==NULL) {
              return -1;
            } else {
              return (intptr_t) cpShapeGetUserData(shape);
            } 
}

