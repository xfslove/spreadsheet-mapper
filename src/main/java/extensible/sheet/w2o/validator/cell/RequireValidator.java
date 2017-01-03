package extensible.sheet.w2o.validator.cell;

import extensible.sheet.model.core.Cell;
import extensible.sheet.model.meta.FieldMeta;
import org.apache.commons.lang3.StringUtils;

/**
 * required validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RequireValidator extends CellValidatorAdapter {

  public RequireValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public RequireValidator(String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
  }

  public RequireValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
  }

  @Override
  public boolean valid(Cell cell, FieldMeta fieldMeta) {
    return customValidate(cell, fieldMeta);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return StringUtils.isNotBlank(cell.getValue());
  }
}
