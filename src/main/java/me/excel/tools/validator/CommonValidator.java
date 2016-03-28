package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

import java.util.function.Function;

/**
 * 自定义validator
 *
 * Created by hanwen on 16-1-7.
 */
public class CommonValidator extends AbstractFieldValidator {

  protected Function<ExcelCell, Boolean> validateResultGetter;

  public CommonValidator(String matchField) {
    super(matchField, null, null);
  }

  public CommonValidator(String matchField, String errorMessage, String prompt) {
    super(matchField, errorMessage, prompt);
  }

  public CommonValidator(String matchField, Function<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, null, null);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonValidator(String matchField, String errorMessage, String prompt, Function<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, errorMessage, prompt);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(excelCell);
  }
}
