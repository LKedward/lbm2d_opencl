#define Q 9

constant float wVec[Q] = {4.0/9.0,1.0/9.0,1.0/9.0,1.0/9.0,1.0/9.0,
                   1.0/36.0,1.0/36.0,1.0/36.0,1.0/36.0};

constant int eVecI[Q] = {0,1,0,-1,0,1,-1,-1,1};
constant int eVecJ[Q] = {0,0,1,0,-1,1,1,-1,-1};

constant int oppK[Q] = {0,3,4,1,2,7,8,5,6};

int idx(int ni, int nj, int i, int j, int k){
// Return a flat index 

  // k varies fastest, then j, then i
  // return i*(nj*Q) + j*Q + k;

  // j varies fastest, then i, then k
  // return k*(ni*nj) + i*nj + j;

  // i varies fastest, then j, then k
  return k*(ni*nj) + j*ni + i;

}

kernel void initialise(const int ni, const int nj, global float * distFun){
// Initialise distribution function

  int i = get_global_id(0);
  int j = get_global_id(1);
  
  if (i>=ni || j>=nj){
    return;
  }

  for (int k=0; k<Q; k++){

    distFun[ idx(ni,nj,i,j,k) ] = wVec[k];

  }

}

kernel void collide(const int ni, const int nj, const float cVel, const float omega,
                        global float * distFun, global int * bcFlags){
// Perform collision operation

  int i = get_global_id(0);
  int j = get_global_id(1);

  if (i>=ni || j>=nj){
    return;
  }

  float rho, ux, uy, temp;
  float Udot, eUdot, eq[Q];
  
  
  rho = 0;
  ux = uy = 0;
  for (int k=0; k<Q; k++){

    temp = distFun[ idx(ni,nj,i,j,k) ];

    // Calculate macroscopic density
    rho += temp;

    // Calculate macroscopic velocities
    ux += cVel*eVecI[k]*temp;
    uy += cVel*eVecJ[k]*temp;

  }

  if (bcFlags[i*nj+j] == 0) {
    ux /= rho;
    uy /= rho;
  }

  // Calculate equilibrium distribution
  for (int k=0; k<Q; k++){

    Udot = ux*ux + uy*uy;
    eUdot = eVecI[k]*ux + eVecJ[k]*uy;

    eq[k] = rho*wVec[k]*(cVel*cVel + 3*eUdot*cVel - 3*Udot/2 + 9*eUdot*eUdot/2)/cVel/cVel;

  }
  
  // Perform collision operator (not on boundaries)
  if (bcFlags[i*nj+j] == 0){
    for (int k=0; k<Q; k++){

      distFun[ idx(ni,nj,i,j,k) ] -= omega*(distFun[ idx(ni,nj,i,j,k) ] - eq[k]);

    }
  } else {
    for (int k=0; k<Q; k++){
      distFun[ idx(ni,nj,i,j,k) ] = 0.0;
    }
  }

}

kernel void boundaryConditions(const int ni, const int nj, global float * distFun,
                          global int * bcFlags, constant float * velocities){
// Apply boundary conditions

  int i = get_global_id(0);
  int j = get_global_id(1);

  if (i>=ni || j>=nj){
    return;
  }

  if (bcFlags[i*nj+j] == -2){
    // Extrapolate from interior

    int nn = 0;
    float temp = 0;
    for (int k=0; k<Q; k++){

      // Neighbour indices
      int in = i + eVecI[k];
      int jn = j + eVecJ[k];

      if (jn>=0 && jn<nj && in>=0 && in <ni ){ // Make sure neighbour exists

        if (bcFlags[in*nj+jn] == 0){ // Make sure neighbour isn't another boundary

          // temp += distFun[idx(ni,nj,in,jn,k)];
          // nn += 1;
          distFun[idx(ni,nj,i,j,k)] = distFun[idx(ni,nj,in,jn,k)];

        }

      }

    }

    // distFun[idx(ni,nj,i,j,k)] = temp/nn;

  } else if (bcFlags[i*nj+j] == -1){
    // Wall (no-slip)

    for (int k=0; k<Q; k++){

      // Neighbour indices
      int in = i + eVecI[k];
      int jn = j + eVecJ[k];

      if (jn>=0 && jn<nj && in>=0 && in <ni ){ // Make sure neighbour exists

        if (bcFlags[in*nj+jn] == 0){ // Make sure neighbour isn't another boundary

          distFun[idx(ni,nj,i,j,k)] = distFun[idx(ni,nj,in,jn,oppK[k])];

        }

      }

    }

  } else if(bcFlags[i*nj+j] > 0){
    // Moving boundary

    float ux = velocities[2*(bcFlags[i*nj+j]-1)];
    float uy = velocities[2*(bcFlags[i*nj+j]-1)+1];

    for (int k=0; k<Q; k++){

      // Neighbour indices
      int in = i + eVecI[k];
      int jn = j + eVecJ[k];

      if (jn>=0 && jn<nj && in>=0 && in <ni ){ // Make sure neighbour exists

        if (bcFlags[in*nj+jn] == 0){ // Make sure neighbour isn't another boundary

          float rhon = 0;
          for (int kk=0; kk<Q; kk++){
            rhon += distFun[ idx(ni,nj,in,jn,kk) ];
          }
          
          float dp = eVecI[k]*ux + eVecJ[k]*uy;

          distFun[idx(ni,nj,i,j,k)] = distFun[idx(ni,nj,in,jn,oppK[k])] + 
                                        2*wVec[k]*rhon*3*dp;

        }

      }

    }
  }

}

kernel void stream(const int ni, const int nj, const global float * distFun, 
                     global float * distFun2, global int * bcFlags){
// Perform streaming operation

  int i = get_global_id(0);
  int j = get_global_id(1);

  if (i>=ni || j>=nj){
    return;
  }

  if (bcFlags[i*nj+j] == 0){
    for (int k=0; k<Q; k++){

      // Neighbour indices
      int in = i - eVecI[k];
      int jn = j - eVecJ[k];

      distFun2[ idx(ni,nj,i,j,k) ] = distFun[ idx(ni,nj,in,jn,k) ] ;

    }
  } else {
    for (int k=0; k<Q; k++){

      distFun2[ idx(ni,nj,i,j,k) ] = 0.0;

    }
  }
}


kernel void macroVars(const int ni, const int nj, const float cVel, global float * distFun, 
                          global float * rho, global float * u, global float * v, global int * bcFlags){
// Calculate macroscopic variables for output

  int i = get_global_id(0);
  int j = get_global_id(1);

  if (i>=ni || j>=nj){
    return;
  }

  float temp1, temp2;

  // Calculate macroscopic density
  temp1 = 0;
  for (int k=0; k<Q; k++){

    temp1 += distFun[ idx(ni,nj,i,j,k) ];

  }
  rho[i*nj+j] = temp1;

  // Calculate macroscopic velocities
  temp1 = temp2 = 0;
  for (int k=0; k<Q; k++){

    temp1 += cVel*eVecI[k]*distFun[ idx(ni,nj,i,j,k) ];
    temp2 += cVel*eVecJ[k]*distFun[ idx(ni,nj,i,j,k) ];

  }

  if (bcFlags[i*nj+j] == 0) {
    temp1 /= rho[i*nj+j];
    temp2 /= rho[i*nj+j];
  }

  u[i*nj+j] = temp1;
  v[i*nj+j] = temp2;

}