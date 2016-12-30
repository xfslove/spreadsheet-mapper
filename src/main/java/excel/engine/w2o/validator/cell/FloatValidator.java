package excel.engine.w2o.validator.cell;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

/**
 * float validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class FloatValidator extends CellValidatorAdapter {

  public FloatValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    try {
      Float.parseFloat(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

}
