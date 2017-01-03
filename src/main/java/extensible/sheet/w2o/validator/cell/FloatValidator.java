package extensible.sheet.w2o.validator.cell;

import extensible.sheet.model.core.Cell;
import extensible.sheet.model.meta.FieldMeta;

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
