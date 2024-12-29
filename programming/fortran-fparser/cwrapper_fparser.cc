//
// Author: Angelo Graziosi
//
//   created   : Sep 12, 2014
//   last edit : Jul 27, 2016
//
//   C Interface to the Function Parser Library
//
// References
//
//   http://fortranwiki.org/fortran/show/Fortran+and+Cpp+objects
//   http://fortranwiki.org/fortran/show/Generating+C+Interfaces
//
// See also:
//
//   http://fortranwiki.org/fortran/show/c_interface_module
//

#include "fparser.hh"

extern "C"
{
  //
  // C interface to FuncionParser (double)
  //
  FunctionParser *FunctionParser__new()
  {
    return new FunctionParser();
  }

  int FunctionParser__Parse(FunctionParser *This,const char *fcn,
			    const char *vars,int useDegrees)
  {
    return This->Parse(fcn,vars,useDegrees);
  }

  void FunctionParser__setDelimiterChar(FunctionParser *This,char c)
  {
    This->setDelimiterChar(c);
  }

  double static_FunctionParser__epsilon()
  {
    return FunctionParser::epsilon();
  }

  void static_FunctionParser__setEpsilon(double e)
  {
    FunctionParser::setEpsilon(e);
  }

  const char* FunctionParser__ErrorMsg(FunctionParser *This)
  {
    return This->ErrorMsg();
  }

  int FunctionParser__GetParseErrorType(FunctionParser *This)
  {
    return This->GetParseErrorType();
  }

  double FunctionParser__Eval(FunctionParser *This,const double *vars)
  {
    return This->Eval(vars);
  }

  int FunctionParser__EvalError(FunctionParser *This)
  {
    return This->EvalError();
  }

  void FunctionParser__Optimize(FunctionParser *This)
  {
    This->Optimize();
  }

  int FunctionParser__AddConstant(FunctionParser *This,
				   const char *name,double value)
  {
    return This->AddConstant(name,value);
  }

  int FunctionParser__AddUnit(FunctionParser *This,
  			      const char *name,double value)
  {
    return This->AddUnit(name,value);
  }

  int FunctionParser__AddFunction(FunctionParser *This,const char *name,
  				  double (*functionPtr)(const double*),
  				  int paramsAmount)
  {
    return This->AddFunction(name,functionPtr,paramsAmount);
  }

  int FunctionParser__AddFunction2(FunctionParser *This,const char *name,
  				  FunctionParser *fp)
  {
    return This->AddFunction(name,(*fp));
  }

  int FunctionParser__RemoveIdentifier(FunctionParser *This,const char *name)
  {
    return This->RemoveIdentifier(name);
  }

  int
  FunctionParser__ParseAndDeduceVariables(FunctionParser *This,
					  const char *fcn,
					  int* amountOfVariablesFound,
					  int useDegrees)
  {
    return This->ParseAndDeduceVariables(fcn,amountOfVariablesFound,
					 useDegrees);
  }

  void FunctionParser__delete(FunctionParser *This)
  {
    delete This;
  }

  //
  // C interface to FuncionParser_cd (complex double)
  //
  FunctionParser_cd *FunctionParser_cd__new()
  {
    return new FunctionParser_cd();
  }

  int FunctionParser_cd__Parse(FunctionParser_cd *This,const char *fcn,
			    const char *vars,int useDegrees)
  {
    return This->Parse(fcn,vars,useDegrees);
  }

  void FunctionParser_cd__setDelimiterChar(FunctionParser_cd *This,char c)
  {
    This->setDelimiterChar(c);
  }

  std::complex<double> static_FunctionParser_cd__epsilon()
  {
    return FunctionParser_cd::epsilon();
  }

  void static_FunctionParser_cd__setEpsilon(std::complex<double> e)
  {
    FunctionParser_cd::setEpsilon(e);
  }

  const char* FunctionParser_cd__ErrorMsg(FunctionParser_cd *This)
  {
    return This->ErrorMsg();
  }

  int FunctionParser_cd__GetParseErrorType(FunctionParser_cd *This)
  {
    return This->GetParseErrorType();
  }

  std::complex<double>
  FunctionParser_cd__Eval(FunctionParser_cd *This,
			  const std::complex<double> *vars)
  {
    return This->Eval(vars);
  }

  int FunctionParser_cd__EvalError(FunctionParser_cd *This)
  {
    return This->EvalError();
  }

  void FunctionParser_cd__Optimize(FunctionParser_cd *This)
  {
    This->Optimize();
  }

  int
  FunctionParser_cd__AddConstant(FunctionParser_cd *This,
				 const char *name,std::complex<double> value)
  {
    return This->AddConstant(name,value);
  }

  int FunctionParser_cd__AddUnit(FunctionParser_cd *This,
  				 const char *name,std::complex<double> value)
  {
    return This->AddUnit(name,value);
  }

  int FunctionParser_cd__AddFunction(FunctionParser_cd *This,const char *name,
    std::complex<double> (*functionPtr)(const std::complex<double>*),
    int paramsAmount)
  {
    return This->AddFunction(name,functionPtr,paramsAmount);
  }

  int FunctionParser_cd__AddFunction2(FunctionParser_cd *This,const char *name,
  				      FunctionParser_cd *fp)
  {
    return This->AddFunction(name,(*fp));
  }

  int FunctionParser_cd__RemoveIdentifier(FunctionParser_cd *This,
					  const char *name)
  {
    return This->RemoveIdentifier(name);
  }

  int
  FunctionParser_cd__ParseAndDeduceVariables(FunctionParser_cd *This,
					  const char *fcn,
					  int* amountOfVariablesFound,
					  int useDegrees)
  {
    return This->ParseAndDeduceVariables(fcn,amountOfVariablesFound,
					 useDegrees);
  }

  void FunctionParser_cd__delete(FunctionParser_cd *This)
  {
    delete This;
  }

}
