package excel.engine.importer.validator.cell;

import excel.engine.model.excel.Cell;

/**
 * float validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class FloatValidator extends CellValidatorAdapter {

  public FloatValidator(String matchField) {
    super(matchField, "应该为小数");
  }

  public FloatValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell) {
    try {
      Float.parseFloat(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

}
