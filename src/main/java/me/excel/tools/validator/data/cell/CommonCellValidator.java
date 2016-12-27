package me.excel.tools.validator.data.cell;


import me.excel.tools.model.excel.ExcelCell;

import java.util.function.Function;

/**
 * customer cell validator
 * <p>
 * Created by hanwen on 16-1-7.
 */
public class CommonCellValidator extends CellValidatorAdapter {

  protected Function<ExcelCell, Boolean> validateResultGetter;

  public CommonCellValidator(String matchField) {
    super(matchField, null);
  }

  public CommonCellValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public CommonCellValidator(String matchField, Function<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, null);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonCellValidator(String matchField, String errorMessage, Function<ExcelCell, Boolean> validateResultGetter) {
    super(matchField, errorMessage);
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
