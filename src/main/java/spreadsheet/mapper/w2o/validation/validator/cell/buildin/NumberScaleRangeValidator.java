package spreadsheet.mapper.w2o.validation.validator.cell.buildin;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.validator.cell.CustomSingleCellValidatorAdapter;

/**
 * number scale range validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class NumberScaleRangeValidator extends CustomSingleCellValidatorAdapter<NumberScaleRangeValidator> {

  private int lte;

  private int gte;

  public NumberScaleRangeValidator lte(int lte) {
    this.lte = lte;
    return getThis();
  }

  public NumberScaleRangeValidator gte(int gte) {
    this.gte = gte;
    return getThis();
  }

  @Override
  protected NumberScaleRangeValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();

    if (!NumberUtils.isNumber(value)) {
      return false;
    }

    String[] numberPlace = value.split("\\.");

    int scale = 0;
    if (numberPlace.length != 1) {
      scale = numberPlace[1].length();
    }

    return gte <= scale && scale <= lte;
  }
}
