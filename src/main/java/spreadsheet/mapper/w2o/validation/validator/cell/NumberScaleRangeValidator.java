package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * number scale range validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class NumberScaleRangeValidator extends CellValidatorAdapter<NumberScaleRangeValidator> {

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

    return getGte() <= scale && scale <= getLte();
  }

  /*=====================
    for customer access
   =====================*/
  protected int getLte() {
    return lte;
  }

  protected int getGte() {
    return gte;
  }
}
