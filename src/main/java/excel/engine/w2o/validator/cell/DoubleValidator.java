package excel.engine.w2o.validator.cell;


import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

/**
 * double validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DoubleValidator extends CellValidatorAdapter {

  public DoubleValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    try {
      Double.parseDouble(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
