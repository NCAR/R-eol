// -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
// vim: set shiftwidth=4 softtabstop=4 expandtab:

/*************************************************************************
 *
 * © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * Confidential & Proprietary 
 *
 *************************************************************************/

#include "align.h"
#include "R_utime.h"

#include <R.h>

/**********************************************************************
 * C Code DOCUMENTATION ************************************************
 **********************************************************************
 NAME time_align

 DESCRIPTION  Align a calendar positions object to new positions.
 To be called from R as 
 \\
 {\tt 
 .Call("time_align", orig.obj, new.pos, c( how, error.how ),
 matchtol)
 },
 where TIMECLASS is replaced by the name of the time class.

 ARGUMENTS
 IARG  time_obj  Original positions object
 IARG  align_pos New positions to align to
 IARG  how_obj   How to perform alignment (see below)
 IARG  match_tol Tolerance for matching

 RETURN Returns an R list object, where each element is a vector of
 the same length as the alignment positions telling what data to take
 from the original ordered data object to make an aligned ordered data
 object.  The first element of 
 the list is a logical vector telling whether or not the output
 should be set to NA for the corresponding alignment position.  The
 second is a logical vector telling whether or not the corresponding
 alignment position should be dropped from the output.  Unless
 the first element of how_obj is ``interp'', the third element of the
 return list is an integer vector of subscripts for the original
 data to form the new data.  If how_obj is ``interp'', the third through
 sixth elements of the return list give the two subscripts to 
 interpolate between and their two weights: the third element is
 the first weight; the fourth is the first subscript; the fifth is
 the second weight; the sixth is the second subscript. 

 ALGORITHM The two positions objects are assumed to be monotonic
 in either direction, with repeats allowed.  This function goes
 through the positions in align_pos, and for each one finds the 
 nearest positions from time_obj before and after the given position.
 If one of the two match within matchtol, after conversion of both
 times to numbers, the corresponding subscript element of the 
 return list is set to the appropriate subscript of
 time_obj.  The first element of how_obj tells what to do if there
 is no match: ``NA'' causes the NA element of the return list to be set
 to true; ``drop'' causes the drop element to be set to true; 
 ``nearest'' puts the subscript of the nearest position into the 
 subscript element; ``before'' uses the position before; ``after''
 uses the position after'', and ``interp'' sets up interpolation 
 weights.  In the last three cases, if the appropriate subscript
 or weights cannot be calculated (i.e. off the end of the series),
 then the second element of how_obj tells what to do: ``NA'', 
 ``drop'', or ``nearest''. 

 EXCEPTIONS 

 See also: num_align

 **********************************************************************/
SEXP utime_align( SEXP time_obj, SEXP align_pos, 
        SEXP how_obj, SEXP match_tol_a )
{

    SEXP ret;

    double diff_under=0, diff_over=0,  align_num;
    double match_tol;

    size_t in_len, in_start, in_curr;
    int in_inc, align_inc;

    size_t align_len, align_start,  align_curr;
    int how, error_how;
    int over_set, under_set;

    Sint *na_data, *drop_data, *sub1_data, *sub2_data=0;
    double *weight1_data=0, *weight2_data=0;

    if(
        (Rf_length(time_obj) < 1) || (Rf_length(align_pos) < 1) ||
        !Rf_isReal( match_tol_a ) || (Rf_length(match_tol_a) < 1) ||
        !Rf_isString( how_obj ) || (Rf_length(how_obj) < 2)
      )
        Rf_error( "Invalid data in c function time_align" ); 

    /* extract input data*/
    eolts::R_utime itime(time_obj);
    double* ivals = itime.getTimePtr();
    in_len = itime.getLength();

    eolts::R_utime atime(align_pos);
    double* avals = atime.getTimePtr();
    align_len = atime.getLength();

    if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "NA" ))
        how = 0;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "drop" ))
        how = 1;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "nearest" ))
        how = 2;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "before" ))
        how = 3;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "after" ))
        how = 4;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 0)), "interp" ))
        how = 5;
    else
        Rf_error( "Invalid third argument in C function time_align" );

    if( !strcmp( CHAR(STRING_ELT(how_obj, 1)), "NA" ))
        error_how = 0;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 1)), "drop" ))
        error_how = 1;
    else if( !strcmp( CHAR(STRING_ELT(how_obj, 1)), "nearest" ))
        error_how = 2;
    else
        Rf_error( "invalid third argument in C function time_align" );

    match_tol = REAL( match_tol_a )[0];
    if(match_tol < 0)
        Rf_error( "invalid fourth argument in C function num_align" );

    /* see if the inputs are increasing or decreasing series */
    in_start = align_start = 0;
    in_inc = align_inc = 1;

    for( in_curr = 1; in_curr < in_len; in_curr++ )
    {
        if(( ivals[in_curr] > ivals[ in_curr - 1 ] ) )
            break;
        if(( ivals[in_curr] < ivals[ in_curr - 1 ] ) )
        {
            in_inc = -1;
            in_start = in_len - 1;
            break;
        }
    }

    for( align_curr = 1; align_curr < align_len; align_curr++ )
    {
        if(( avals[align_curr] > avals[ align_curr - 1 ] ) )
            break;
        if(( avals[align_curr] < avals[ align_curr - 1 ] ) )
        {
            align_inc = -1;
            align_start = align_len - 1;
            break;
        }
    }

    /* create return list */

    if( how == 5 )
    {
        PROTECT(ret = Rf_allocVector(VECSXP,6));
        na_data = LOGICAL( SET_VECTOR_ELT(ret, 0 , Rf_allocVector(LGLSXP,align_len)) );
        drop_data = LOGICAL( SET_VECTOR_ELT(ret, 1 , Rf_allocVector(LGLSXP,align_len)) );
        weight1_data = REAL( SET_VECTOR_ELT(ret, 2 , Rf_allocVector(REALSXP,align_len)) );
        sub1_data = INTEGER( SET_VECTOR_ELT(ret, 3 , Rf_allocVector(INTSXP,align_len)) );
        weight2_data = REAL( SET_VECTOR_ELT(ret, 4 , Rf_allocVector(REALSXP,align_len)) );
        sub2_data = INTEGER( SET_VECTOR_ELT(ret, 5 , Rf_allocVector(INTSXP,align_len)) );
    } else
    {
        PROTECT(ret = Rf_allocVector(VECSXP,3));
        na_data = LOGICAL( SET_VECTOR_ELT(ret, 0 , Rf_allocVector(LGLSXP,align_len)) );
        drop_data = LOGICAL( SET_VECTOR_ELT(ret, 1 , Rf_allocVector(LGLSXP,align_len)) );
        sub1_data = INTEGER( SET_VECTOR_ELT(ret, 2 , Rf_allocVector(INTSXP,align_len)) );
    }

    /* go through the alignment positions and find the right indexes,
       NA or not values, and drop values */

    in_curr = in_start;

    /* goes from:
     *  0 to align_len-1 
     *  align_len-1 to 0
     */
    for( align_curr = align_start; align_curr < align_len; align_curr += align_inc )
    {
        /* move along the input series until we pass current align position */
        while( ( in_curr < in_len ) && ( ivals[ in_curr ] < avals[ align_curr ] ) )
            in_curr += in_inc;
        /* in_curr will be in the range:
         *  in_start=0 to in_len for in_inc==1
         *  in_start=(inlen-1) to 0 to MAX_UINT (wrap-around), for in_inc= -1
         */
        /* see how far we are from the current position */

        align_num = avals[ align_curr ];

        /* over_set and under_set get set if we're inside the respective ends of 
           the time series; diff_over and diff_under store the difference
           between the align_ time and the in_ times before and after it. */

        over_set = under_set = 0;
        if( ( in_curr < in_len ))
        {
            double over_num = ivals[ in_curr ];
            diff_over = over_num - align_num; 
            over_set = 1;
        }

        if( ( in_curr != in_start ) )
        {
            double under_num = ivals[ in_curr - in_inc ];

            diff_under = align_num - under_num; 
            under_set = 1;
        }

        na_data[ align_curr ] = 0;
        drop_data[ align_curr ] = 0;
        sub1_data[ align_curr ] = 1;
        if( how == 5 )
        {
            weight1_data[ align_curr ] = 1.0;
            sub2_data[ align_curr ] = 1;
            weight2_data[ align_curr ] = 0.0;
        }

        /* is it a match? */
        if( under_set && 
                ( !over_set || ( diff_under < diff_over )) && /* better than over */
                ( diff_under <= match_tol ))
            sub1_data[ align_curr ] = 1 + in_curr - in_inc; /* matches under */
        else if( over_set && ( diff_over <= match_tol ))
            sub1_data[ align_curr ] = 1 + in_curr;  /* matches over */
        else if(( how == 0 ) || 
                (( error_how == 0 ) &&
                 (( !under_set && (( how == 3 ) || ( how == 5 ))) ||
                  ( !over_set && (( how == 4 ) || ( how == 5 ))))))
            na_data[ align_curr ] = 1;  /* make it an NA on no match */
        else if(( how == 1 ) || 
                (( error_how == 1 ) &&
                 (( !under_set && (( how == 3 ) || ( how == 5 ))) ||
                  ( !over_set && (( how == 4 ) || ( how == 5 ))))))
            drop_data[ align_curr ] = 1;  /* drop on no match */
        else if(( how == 2 ) || 
                ((( error_how == 2 ) &&
                  (( !under_set && (( how == 3 ) || ( how == 5 ))) ||
                  ( !over_set && (( how == 4 ) || ( how == 5 )))))))
        {
            /* take nearest on no match */
            if( !under_set )  /* one end */
                sub1_data[ align_curr ] = 1 + in_curr;
            else if( !over_set ) /* other end */
                sub1_data[ align_curr ] = 1 + in_curr - in_inc;
            else if( diff_under <= diff_over ) 
                sub1_data[ align_curr ] = 1 + in_curr - in_inc;
            else
                sub1_data[ align_curr ] = 1 + in_curr;
        } else if( how == 3 ) /* before */
            sub1_data[ align_curr ] = 1 + in_curr - in_inc;
        else if( how == 4 ) /* after */
            sub1_data[ align_curr ] = 1 + in_curr;
        else /* interp */
        {
            sub1_data[ align_curr ] = 1 + in_curr;
            sub2_data[ align_curr ] = 1 + in_curr - in_inc;
            weight1_data[ align_curr ] = diff_under / ( diff_over + diff_under );
            weight2_data[ align_curr ] = diff_over / ( diff_over + diff_under );
        }
    }

    UNPROTECT(1);

    return( ret );
}
