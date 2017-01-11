package spreadsheet.mapper.w2o.validator.cell;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validator.row.MultiUniqueInImportFileValidator;

import java.util.HashSet;
import java.util.Set;

/**
 * <pre>
 * value unique in template validator
 *
 * like {@link MultiUniqueInImportFileValidator},
 * this validator only check one cell value if unique.
 * </pre>
 * Created by hanwen on 2016/12/1.
 */
public class UniqueInImportFileValidator extends CellValidatorAdapter {

  private Set<String> cellValueHolder = new HashSet<>();

  public UniqueInImportFileValidator(String matchField, String errorMessage) {
    this(matchField, errorMessage, null);
  }

  public UniqueInImportFileValidator(String matchField, String errorMessage, String[] dependsOn) {
    this(matchField, matchField, errorMessage, dependsOn);
  }

  public UniqueInImportFileValidator(String group, String matchField, String errorMessage, String[] dependsOn) {
    this(group, matchField, errorMessage, matchField, dependsOn);
  }

  public UniqueInImportFileValidator(String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {

    String cellValue = cell.getValue();
    if (StringUtils.isBlank(cellValue)) {
      return true;
    }

    if (cellValueHolder.contains(cellValue)) {
      return false;
    }

    cellValueHolder.add(cellValue);
    return true;

  }

}
