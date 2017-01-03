package spread.sheet.w2o.validator.cell;

import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * number scale range validator
 * <p>
 * Created by hanwen on 2016/12/1.
 */
public class NumberScaleRangeValidator extends CellValidatorAdapter {

  private int lte;

  private int gte;

  public NumberScaleRangeValidator(int lte, int gte, String matchField, String errorMessage) {
    super(matchField, errorMessage);
    this.gte = gte;
    this.lte = lte;
  }

  public NumberScaleRangeValidator(int lte, int gte, String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
    this.gte = gte;
    this.lte = lte;
  }

  public NumberScaleRangeValidator(int lte, int gte, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
    this.gte = gte;
    this.lte = lte;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {

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
