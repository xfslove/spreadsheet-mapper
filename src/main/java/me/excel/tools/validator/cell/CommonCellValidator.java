package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import me.excel.tools.validator.ValidateFunction;

/**
 * customer cell validator
 * <p>
 * Created by hanwen on 16-1-7.
 */
public class CommonCellValidator extends CellValidatorAdapter {

  protected ValidateFunction<ExcelCell, Boolean> validateResultGetter;

  public CommonCellValidator(String matchField) {
    super(matchField, null);
  }

  public CommonCellValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public CommonCellValidator(String matchField, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, null);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonCellValidator(String matchField, String errorMessage, ValidateFunction<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, errorMessage);
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
