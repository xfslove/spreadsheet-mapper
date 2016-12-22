package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;
import me.excel.tools.validator.ValidateFunction;

/**
 * customer row validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class CommonRowValidator extends RowValidatorAdapter {

  protected ValidateFunction<ExcelRow, Boolean> validateResultGetter;

  public CommonRowValidator(ValidateFunction<ExcelRow, Boolean> validateResultGetter, String errorMessage, String[] messageOnFields) {
    super(errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(ValidateFunction<ExcelRow, Boolean> validateResultGetter, String[] messageOnFields) {
    super(null, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(ExcelRow excelRow) throws SkipValidateException {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(excelRow);
  }
}
