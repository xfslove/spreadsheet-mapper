package me.excel.tools.validator.data.row;


import me.excel.tools.model.excel.ExcelRow;

import java.util.function.Function;

/**
 * customer row validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class CommonRowValidator extends RowValidatorAdapter {

  private Function<ExcelRow, Boolean> validateResultGetter;

  public CommonRowValidator(Function<ExcelRow, Boolean> validateResultGetter, String errorMessage, String[] messageOnFields) {
    super(errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(Function<ExcelRow, Boolean> validateResultGetter, String[] messageOnFields) {
    super(null, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(ExcelRow excelRow) {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(excelRow);
  }
}
