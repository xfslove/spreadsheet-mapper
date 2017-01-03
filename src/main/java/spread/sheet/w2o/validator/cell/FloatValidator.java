package spread.sheet.w2o.validator.cell;

import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.core.Cell;

/**
 * float validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class FloatValidator extends CellValidatorAdapter {

  public FloatValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public FloatValidator(String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
  }

  public FloatValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
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
