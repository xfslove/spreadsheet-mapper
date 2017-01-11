package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * required validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RequireValidator extends CellValidatorAdapter {

  public RequireValidator(String matchField, String errorMessage) {
    this(matchField, errorMessage, null);
  }

  public RequireValidator(String matchField, String errorMessage, String[] dependsOn) {
    this(matchField, matchField, errorMessage, dependsOn);
  }

  public RequireValidator(String group, String matchField, String errorMessage, String[] dependsOn) {
    this(group, matchField, errorMessage, matchField, dependsOn);
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
