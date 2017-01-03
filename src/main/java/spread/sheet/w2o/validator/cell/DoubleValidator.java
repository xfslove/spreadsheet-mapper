package spread.sheet.w2o.validator.cell;


import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.core.Cell;

/**
 * double validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class DoubleValidator extends CellValidatorAdapter {

  public DoubleValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public DoubleValidator(String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
  }

  public DoubleValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
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
