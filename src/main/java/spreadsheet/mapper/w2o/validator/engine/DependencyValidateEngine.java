package spreadsheet.mapper.w2o.validator.engine;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.message.ErrorMessage;
import spreadsheet.mapper.model.message.ErrorMessageBean;
import spreadsheet.mapper.model.message.MessageWriteStrategies;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.validator.DependencyValidator;
import spreadsheet.mapper.w2o.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validator.row.RowValidator;

import java.util.*;

/**
 * use simply dfs to do dependency validate, depends on dependencies no cycle {@link DependencyCycleCheckEngine}
 * <p>
 * Created by hanwen on 2017/1/6.
 */
public class DependencyValidateEngine {

  private Map<String, List<DependencyValidator>> validatorMap = new LinkedHashMap<>();
  private SheetMeta sheetMeta;
  private Row row;

  private Map<String, Set<String>> vGraph = new LinkedHashMap<>();
  private Map<String, Boolean> visited = new HashMap<>();

  // is skip valid rest validator in one search tree
  private boolean skip;

  private List<ErrorMessage> errorMessages = new ArrayList<>();

  public DependencyValidateEngine(Map<String, List<DependencyValidator>> validatorMap, SheetMeta sheetMeta, Row row) {
    this.vGraph = DependencyEngineHelper.buildVGraph(validatorMap);
    for (String v : vGraph.keySet()) {
      visited.put(v, false);
    }
    this.validatorMap = validatorMap;
    this.sheetMeta = sheetMeta;
    this.row = row;
  }

  public void valid() {

    for (String v : vGraph.keySet()) {
      skip = false;
      if (!visited.get(v)) {
        dfs(v);
      }
    }
  }

  public List<ErrorMessage> getErrorMessages() {
    return errorMessages;
  }

  private void dfs(String v) {

    for (String w : vGraph.get(v)) {
      if (!visited.get(w)) {
        dfs(w);
      }
    }

    visited.put(v, true);

    if (skip) {
      return;
    }

    List<DependencyValidator> validators = validatorMap.get(v);

    for (DependencyValidator validator : validators) {
      boolean vResult = doRowCellsValid(validator);
      // if one of the group valid failure skip rest validators
      if (!vResult) {
        skip = true;
        break;
      }
    }
  }

  private boolean doRowCellsValid(DependencyValidator dependencyValidator) {
    if (dependencyValidator instanceof RowValidator) {

      RowValidator rowValidator = (RowValidator) dependencyValidator;
      boolean result = rowValidator.valid(row, sheetMeta);

      if (!result) {
        for (String messageOnField : rowValidator.getMessageOnFields()) {
          FieldMeta fieldMeta = sheetMeta.getFieldMeta(messageOnField);

          errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, rowValidator.getErrorMessage(), row.getSheet().getIndex(), row.getIndex(), fieldMeta.getColumnIndex()));
        }
      }

      return result;
    } else {

      CellValidator cellValidator = (CellValidator) dependencyValidator;
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(cellValidator.getMatchField());

      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      boolean result = cellValidator.valid(cell, fieldMeta);

      if (!result) {
        String messageOnField = cellValidator.getMessageOnField();
        FieldMeta messageOnFieldMeta = sheetMeta.getFieldMeta(messageOnField);
        Cell messageOnCell = row.getCell(messageOnFieldMeta.getColumnIndex());

        errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, cellValidator.getErrorMessage(), row.getSheet().getIndex(), row.getIndex(), messageOnCell.getIndex()));
      }

      return result;
    }
  }
}
