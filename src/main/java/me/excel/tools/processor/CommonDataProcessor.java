package me.excel.tools.processor;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * customer data processor
 * <p>
 * Created by hanwen on 16-1-18.
 */
public class CommonDataProcessor<D> implements DataProcessor {

  private Consumer<D> handleAcceptor;

  public CommonDataProcessor(Consumer<D> handleAcceptor) {
    this.handleAcceptor = handleAcceptor;
  }

  @Override
  public void preProcessing(Object origin) {
    // nothing
  }

  @Override
  public void postProcessing(Object model) {
    // nothing
  }

  @Override
  public void handle(List models) {
    Optional<Consumer<D>> handleAcceptor = Optional.ofNullable(this.handleAcceptor);
    handleAcceptor.ifPresent(dConsumer -> models.forEach(model -> dConsumer.accept((D) model)));
  }
}
