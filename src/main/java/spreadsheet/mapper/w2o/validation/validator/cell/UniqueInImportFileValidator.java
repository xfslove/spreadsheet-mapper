package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.validator.row.MultiUniqueInImportFileValidator;

import java.util.HashSet;
import java.util.Set;

/**
 * <pre>
 * value unique in template validator
 *
 * like {@link MultiUniqueInImportFileValidator},
 * this validator only check one cell value if unique.
 * </pre>
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class UniqueInImportFileValidator extends CellValidatorAdapter<UniqueInImportFileValidator> {

  private Set<String> cellValueHolder = new HashSet<>();

  @Override
  protected UniqueInImportFileValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {

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
