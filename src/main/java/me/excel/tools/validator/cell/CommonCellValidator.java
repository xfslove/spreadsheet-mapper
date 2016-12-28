package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.Cell;

import java.util.function.Function;

/**
 * customer cell validator
 * <p>
 * Created by hanwen on 16-1-7.
 */
public class CommonCellValidator extends CellValidatorAdapter {

  protected Function<Cell, Boolean> validateResultGetter;

  public CommonCellValidator(String matchField) {
    super(matchField, null);
  }

  public CommonCellValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public CommonCellValidator(String matchField, Function<Cell, Boolean> validateResultGetter) {
    super(matchField, null);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonCellValidator(String matchField, String errorMessage, Function<Cell, Boolean> validateResultGetter) {
    super(matchField, errorMessage);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(Cell cell) {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(cell);
  }
}
