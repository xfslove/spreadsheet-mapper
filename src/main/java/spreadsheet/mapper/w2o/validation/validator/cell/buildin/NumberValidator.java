package spreadsheet.mapper.w2o.validation.validator.cell.buildin;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.validator.cell.CustomSingleCellValidatorAdapter;

/**
 * number validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class NumberValidator extends CustomSingleCellValidatorAdapter<NumberValidator> {

  @Override
  protected NumberValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    return NumberUtils.isNumber(cell.getValue());
  }
}
