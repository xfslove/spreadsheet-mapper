package excel.engine.importer.validator.cell;


import excel.engine.model.excel.Cell;

/**
 * double validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DoubleValidator extends CellValidatorAdapter {

  public DoubleValidator(String matchField) {
    super(matchField, "应该为小数");
  }

  public DoubleValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell) {
    try {
      Double.parseDouble(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
