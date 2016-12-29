package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.Cell;
import org.apache.commons.lang3.StringUtils;

import java.excel.engine.importer.validator.row.MultiUniqueInImportFileValidator;
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

  public UniqueInImportFileValidator(String matchField) {
    super(matchField, "导入文件中存在重复数据");
  }

  public UniqueInImportFileValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell) {

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
