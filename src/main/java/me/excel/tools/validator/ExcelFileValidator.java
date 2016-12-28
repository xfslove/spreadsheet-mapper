package me.excel.tools.validator;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.exception.ExcelValidateException;
import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;
import me.excel.tools.model.excel.Workbook;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.sheet.SheetValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;
import org.apache.commons.collections.CollectionUtils;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileValidator implements UserFileValidator {

  /*===============
    validators
   ================*/
  private List<WorkbookValidator> workbookValidators = new ArrayList<>();
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private List<RowValidator> rowValidators = new ArrayList<>();
  private List<CellValidator> cellValidators = new ArrayList<>();

  /*==============
    error messages TODO
   ===============*/
  private List<DataValidateMessage> errorMessages = new ArrayList<>();

  private Workbook workbook;

  public ExcelFileValidator(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public void addWorkbookValidator(WorkbookValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.workbookValidators, validators);
  }

  @Override
  public void addSheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.sheetValidators, validators);
  }

  @Override
  public void addRowValidator(RowValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.rowValidators, validators);
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.cellValidators, validators);
  }

  @Override
  public boolean valid() {
    if (workbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    // check dependency
    checkValidatorKeyDependency();

    // valid data
    validWorkbook(workbook);
    if (CollectionUtils.isNotEmpty(errorMessages)) {
      return false;
    }

    for (Sheet sheet : workbook.getSheets()) {

      validSheet(sheet);

      if (CollectionUtils.isNotEmpty(errorMessages)) {
        return false;
      }

      for (Row row : sheet.getRows()) {

        validRowCells(row);
      }
    }

    return CollectionUtils.isEmpty(errorMessages);
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependency() {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(rowValidators));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(cellValidators));

    Set<String> satisfiedKeys = new HashSet<>();
    for (String key : dependsOnHierarchy.keySet()) {
      Set<String> dependencyKeys = new HashSet<>();
      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, key);
      satisfiedKeys.addAll(dependencyKeys);
    }
  }

  private void checkValidatorKeyDependencyHierarchy(
      Map<String, Set<String>> dependsOnHierarchy,
      Set<String> satisfiedKeys,
      Set<String> dependencyKeys,
      String key
  ) {

    if (satisfiedKeys.contains(key)) {
      return;
    }

    dependencyKeys.add(key);
    for (String dependsOn : dependsOnHierarchy.get(key)) {

      if (!dependsOnHierarchy.containsKey(dependsOn)) {
        throw new ExcelValidateException("dependency missing key [" + dependsOn + "]");
      }

      if (dependencyKeys.contains(dependsOn)) {
        throw new ExcelValidateException("dependency cycling on [" + key + "] and [" + dependsOn + "]");
      }

      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, dependsOn);
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validWorkbook(Workbook workbook) {

    for (WorkbookValidator validator : workbookValidators) {

      if (!validator.validate(workbook)) {

      }
    }

  }

  private void validSheet(Sheet sheet) {

    for (SheetValidator validator : sheetValidators) {

      if (!validator.validate(sheet)) {

      }
    }

  }

  private void validRowCells(Row row) {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(rowValidators));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(cellValidators));

    // one key corresponding multi validators
    Map<String, List<DataValidator>> validatorMap = new HashMap<>();

    validatorMap.putAll(buildValidatorMap(rowValidators));
    validatorMap.putAll(buildValidatorMap(cellValidators));

    Map<String, Set<Boolean>> allResult = new HashMap<>();

    for (RowValidator validator : rowValidators) {
      allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getKey()));
    }
    for (CellValidator validator : cellValidators) {
      allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getKey()));
    }

  }

  private Map<String, Set<Boolean>> validRowCellsHierarchy(
      Map<String, List<DataValidator>> validatorMap,
      Map<String, Set<Boolean>> allResult,
      Map<String, Set<String>> dependsOnHierarchy,
      Row row,
      String key
  ) {

    Map<String, Set<Boolean>> result = new HashMap<>();

    if (allResult.containsKey(key)) {
      result.put(key, allResult.get(key));
      return result;
    }

    Set<String> dependsOns = dependsOnHierarchy.get(key);

    if (CollectionUtils.isNotEmpty(dependsOns)) {

      for (String dependsOn : dependsOns) {

        result.putAll(validRowCellsHierarchy(validatorMap, result, dependsOnHierarchy, row, dependsOn));
      }

    }

    if (ifSkip(result)) {
      result.put(key, Collections.singleton(null));
      return result;
    }

    Set<Boolean> vrs = new HashSet<>();
    for (DataValidator dataValidator : validatorMap.get(key)) {
      vrs.add(doRowCellsValid(dataValidator, row));
    }
    result.put(key, vrs);
    return result;
  }

  private boolean ifSkip(Map<String, Set<Boolean>> result) {

    Set<Boolean> dependsOnVrs = new HashSet<>();
    for (Set<Boolean> vr : result.values()) {
      dependsOnVrs.addAll(vr);
    }

    return dependsOnVrs.contains(Boolean.FALSE) || dependsOnVrs.contains(null);
  }

  private boolean doRowCellsValid(DataValidator dataValidator, Row row) {

    if (dataValidator instanceof RowValidator) {

      return ((RowValidator) dataValidator).validate(row);
    } else {

      CellValidator cellValidator = (CellValidator) dataValidator;
      return cellValidator.validate(row.getCell(cellValidator.getMatchField()));
    }
  }

  private <VALIDATOR extends DataValidator> Map<String, Set<String>> buildDependsOnHierarchy(List<VALIDATOR> dataValidators) {
    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    for (VALIDATOR validator : dataValidators) {
      String key = validator.getKey();
      Set<String> dependsOn = validator.getDependsOn();

      if (!dependsOnHierarchy.containsKey(key)) {
        dependsOnHierarchy.put(key, dependsOn);
      }
      dependsOnHierarchy.get(key).addAll(dependsOn);
    }

    return dependsOnHierarchy;
  }

  private <VALIDATOR extends DataValidator> Map<String, List<DataValidator>> buildValidatorMap(List<VALIDATOR> validators) {

    Map<String, List<DataValidator>> validatorMap = new HashMap<>();

    for (VALIDATOR validator : validators) {
      String key = validator.getKey();
      if (!validatorMap.containsKey(key)) {
        validatorMap.put(key, new ArrayList<>());
      }
      validatorMap.get(key).add(validator);
    }

    return validatorMap;
  }

}
