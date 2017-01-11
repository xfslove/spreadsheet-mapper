package spreadsheet.mapper.w2o.validator.cell;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * number validator
 * <p>
 * Created by hanwen on 16/7/7.
 */
public class NumberValidator extends CellValidatorAdapter {

  public NumberValidator(String matchField, String errorMessage) {
    this(matchField, errorMessage, null);
  }

  public NumberValidator(String matchField, String errorMessage, String[] dependsOn) {
    this(matchField, matchField, errorMessage, dependsOn);
  }

  public NumberValidator(String group, String matchField, String errorMessage, String[] dependsOn) {
    this(group, matchField, errorMessage, matchField, dependsOn);
  }

  public NumberValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return NumberUtils.isNumber(cell.getValue());
  }
}
