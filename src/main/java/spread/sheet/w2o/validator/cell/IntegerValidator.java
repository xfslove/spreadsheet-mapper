package spread.sheet.w2o.validator.cell;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import spread.sheet.Constants;
import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

/**
 * integer number validator
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public class IntegerValidator extends CellValidatorAdapter {

  public IntegerValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  public IntegerValidator(String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
  }

  public IntegerValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();
    if (StringUtils.startsWith(value, Constants.NEGATIVE_SEPARATOR)) {
      value = StringUtils.substring(value, 1, value.length());
    }
    return NumberUtils.isDigits(value);
  }
}
