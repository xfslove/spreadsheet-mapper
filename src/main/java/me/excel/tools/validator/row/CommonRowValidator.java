package me.excel.tools.validator.row;


import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;
import me.excel.tools.validator.ValidateFunction;

/**
 * Created by hanwen on 4/26/16.
 */
public class CommonRowValidator extends AbstractRowValidator {

  protected ValidateFunction<ExcelRow, Boolean> validateResultGetter;

  public CommonRowValidator(ValidateFunction<ExcelRow, Boolean> validateResultGetter, String prompt, String errorMessage, String[] messageOnFields) {
    super(prompt, errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(ValidateFunction<ExcelRow, Boolean> validateResultGetter, String errorMessage,  String[] messageOnFields) {
    super(null, errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(ValidateFunction<ExcelRow, Boolean> validateResultGetter,  String[] messageOnFields) {
    super(null, null, messageOnFields);
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
