package me.excel.tools.processor;

import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by hanwen on 16-1-18.
 */
public class CommonDataProcessor<D> implements DataProcessor {

  protected Consumer<D> preAcceptor;

  protected BiConsumer<D, D> postAcceptor;

  protected Consumer<D> handleAcceptor;

  public CommonDataProcessor(Consumer<D> handleAcceptor) {
    this.handleAcceptor = handleAcceptor;
  }

  @Override
  public void preProcessing(Object model) {
    Optional<Consumer<D>> preAcceptor = Optional.ofNullable(this.preAcceptor);
    if (preAcceptor.isPresent()) {
      preAcceptor.get().accept((D) model);
    }
  }

  @Override
  public void postProcessing(Object origin, Object model) {
    Optional<BiConsumer<D, D>> postAcceptor = Optional.ofNullable(this.postAcceptor);
    if (postAcceptor.isPresent()) {
      postAcceptor.get().accept((D) origin, (D) model);
    }
  }

  @Override
  public void handle(List models) {
    Optional<Consumer<D>> handleAcceptor = Optional.ofNullable(this.handleAcceptor);
    if (handleAcceptor.isPresent()) {
      models.forEach(model -> handleAcceptor.get().accept((D) model));
    }
  }
}
