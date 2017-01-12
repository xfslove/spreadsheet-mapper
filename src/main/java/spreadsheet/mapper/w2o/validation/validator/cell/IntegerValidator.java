package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * integer number validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class IntegerValidator extends CellValidatorAdapter<IntegerValidator> {

  @Override
  protected IntegerValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();
    if (StringUtils.startsWith(value, Constants.NEGATIVE_SEPARATOR)) {
      value = StringUtils.substring(value, 1, value.length());
    }
    return NumberUtils.isDigits(value);
  }
}
