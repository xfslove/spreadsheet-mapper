package spread.sheet.w2o.validator.cell;


import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.core.Cell;

/**
 * int validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class IntValidator extends CellValidatorAdapter {

  public IntValidator(String matchField, String message) {
    super(matchField, message);
  }

  public IntValidator(String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
  }

  public IntValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    try {
      Integer.parseInt(cell.getValue());
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }
}
