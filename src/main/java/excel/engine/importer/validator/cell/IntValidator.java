package excel.engine.importer.validator.cell;


import excel.engine.model.excel.Cell;

/**
 * int validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class IntValidator extends CellValidatorAdapter {

  public IntValidator(String matchField) {
    super(matchField, "应该为整数");
  }

  public IntValidator(String matchField, String message) {
    super(matchField, message);
  }

  @Override
  protected boolean customValidate(Cell cell) {
    try {
      Integer.parseInt(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
