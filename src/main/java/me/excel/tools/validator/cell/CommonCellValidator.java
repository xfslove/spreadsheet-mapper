package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import me.excel.tools.validator.ValidateFunction;

/**
 * 自定义validator
 *
 * Created by hanwen on 16-1-7.
 */
public class CommonCellValidator extends AbstractCellValidator {

  protected ValidateFunction<ExcelCell, Boolean> validateResultGetter;

  public CommonCellValidator(String matchField) {
    super(matchField, null, null);
  }

  public CommonCellValidator(String matchField, String errorMessage, String prompt) {
    super(matchField, errorMessage, prompt);
  }

  public CommonCellValidator(String matchField, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, null, null);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonCellValidator(String matchField, String errorMessage, String prompt, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, errorMessage, prompt);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(excelCell);
  }
}
