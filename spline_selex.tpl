// A function to explore cubic spline selectivity as implemented in Stock Synthesis
// It is to be called by the selfit_spline R function available from http://code.google.com/p/r4ss/
// by Ian Taylor
// based on example code provided by Mark Payne 
// at http://groups.google.com/group/admb-users/browse_thread/thread/e6ba196af3478dc0/1952dcc0dfbb6241?#1952dcc0dfbb6241
// who in turn was aided by Derek Seiple
 

DATA_SECTION
  init_int Nknots
  init_vector knotsX(1,Nknots)
  init_vector knotsY(1,Nknots)
  init_vector parm(1,2)
  init_int nlength
  init_vector len_bins_m(1,nlength)
  // counters
  int i
  int j2
  int z
  number temp


PARAMETER_SECTION

  //init_bounded_vector knotsY(1,Nknots)
  init_bounded_number dummy(0,2,1)
  vector tempvec_l(1,nlength)
  vector tempvec_l2(1,nlength)
  vector sel(1,nlength)
  objective_function_value obj_fun


PROCEDURE_SECTION

  dummy = 1;
  // define splines
  dvector splineX(1,Nknots);
  dvar_vector splineY(1,Nknots);

  //... Populate splineX and splineY ...//
  splineX = knotsX;
  splineY = knotsY;
  
  //Create the class
  vcubic_spline_function splinefn=vcubic_spline_function(splineX,splineY,parm(1),parm(2));

  //Evaluate the function
  tempvec_l = splinefn(len_bins_m); 
  tempvec_l2 = tempvec_l;

  // calculated selectivity as in SS
  z=nlength;
  while(len_bins_m(z)>splineX(Nknots)) {z--;}
  j2=z+1;  //  first size bin beyond last node
  temp = value(max(tempvec_l(1,j2)));

  tempvec_l-=temp;  // rescale to get max of 0.0
  tempvec_l(j2+1,nlength) = tempvec_l(j2);  //  set constant above last node
  sel = mfexp(tempvec_l);

  // objective function required to make ADMB work (I think)
  obj_fun=(dummy-1)*(dummy-1); 

  ofstream Output("spline_selex.txt");   // this is just a create
  Output<<"#shift: "<<temp<<endl;
  Output<<"#len  spline  sel"<<endl;
  for(i=1; i<=nlength; i++) Output<<len_bins_m(i)<<"  "<<tempvec_l2(i)<<"  "<<sel(i)<<endl;


REPORT_SECTION
