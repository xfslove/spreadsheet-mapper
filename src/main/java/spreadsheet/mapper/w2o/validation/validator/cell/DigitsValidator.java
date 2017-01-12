package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * digits validator
 * <p>
 * Created by hanwen on 2017/1/12.
 */
public class DigitsValidator extends CellValidatorAdapter<DigitsValidator> {

  @Override
  protected DigitsValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    return NumberUtils.isDigits(cell.getValue());
  }
}
