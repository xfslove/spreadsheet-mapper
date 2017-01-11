package spreadsheet.mapper.w2o.validator.cell;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * integer number validator
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public class IntegerValidator extends CellValidatorAdapter {

  public IntegerValidator(String matchField, String errorMessage) {
    this(matchField, errorMessage, null);
  }

  public IntegerValidator(String matchField, String errorMessage, String[] dependsOn) {
    this(matchField, matchField, errorMessage, dependsOn);
  }

  public IntegerValidator(String group, String matchField, String errorMessage, String[] dependsOn) {
    this(group, matchField, errorMessage, matchField, dependsOn);
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
