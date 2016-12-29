package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.Cell;

/**
 * long validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LongValidator extends CellValidatorAdapter {

  public LongValidator(String matchField) {
    super(matchField, "应该为整数");
  }

  public LongValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell) {
    try {
      Long.parseLong(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
