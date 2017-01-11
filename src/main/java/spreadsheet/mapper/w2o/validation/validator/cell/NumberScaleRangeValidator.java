package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.math.NumberUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * number scale range validator
 * <p>
 * Created by hanwen on 2016/12/1.
 */
public class NumberScaleRangeValidator extends CellValidatorAdapter {

  private int lte;

  private int gte;

  public NumberScaleRangeValidator(int gte, int lte, String matchField, String errorMessage) {
    this(gte, lte, matchField, errorMessage, null);
  }

  public NumberScaleRangeValidator(int gte, int lte, String matchField, String errorMessage, String[] dependsOn) {
    this(gte, lte, matchField, matchField, errorMessage, dependsOn);
  }

  public NumberScaleRangeValidator(int gte, int lte, String group, String matchField, String errorMessage, String[] dependsOn) {
    this(gte, lte, group, matchField, errorMessage, matchField, dependsOn);
  }

  public NumberScaleRangeValidator(int gte, int lte, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
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
